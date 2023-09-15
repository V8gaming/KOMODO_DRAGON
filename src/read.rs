use std::{collections::HashMap, sync::Mutex, fs::File, io::{Error,ErrorKind, stdin}};

use lazy_static::lazy_static;
lazy_static! {
    pub static ref FILE_MAP: Mutex<HashMap<i32, Result<FileData, Error>>> = Mutex::new(HashMap::new());
    pub static ref STATEMENT_MAP: Mutex<HashMap<i32, String>> = Mutex::new(HashMap::new());
}

pub type FileData = (Option<File>, FileMeta);
#[derive(Debug, PartialEq)]
/// Default is unknown
/// if TMPDIR environment variable is set to a writable directory, then the temporary file is created in that directory; otherwise, the temporary file is created in the current working directory.
pub enum Status {
    Old, // The file already exists (nonexistence is an error)
    New, // The file does not exist (existence is an error), if filename is Asterisk, then file is, when n is the unit number, rust.n
    Unknown, //Existence is unknown. 
    Scratch, // File is temporary, with the name of the form tmp.FAAAxnnnnn, and is deleted when closed,  To prevent deletion, CLOSE with STATUS='KEEP'.
    Replace, // If REPLACE is specified and the file does not already exist, the file is created and the status is changed to OLD.
}

#[derive(PartialEq)]
/// Default is sequential access
/// Only directly accessible files are allowed; thus, tty, pipes, and magnetic tape are not allowed. If you build a file as sequential, then you cannot access it as direct.
/// If FORM is not specified, unformatted transfer is assumed.
/// 
/// If FORM='UNFORMATTED', the size of each transfer depends upon the data transferred.
/// 
/// If ACCESS='SEQUENTIAL', RECL is ignored. @ The FORTRAN 77 Standard prohibits RECL for sequential access.
/// 
/// No padding of records is done.
/// 
/// If you build a file as direct, then you cannot access it as sequential.
/// 
/// Files do not have to be randomly accessible, in the sense that tty, pipes, and tapes can be used. For tapes, we recommend the TOPEN() routines because they are more reliable.
/// 
/// If FORM is not , formatted transfer is assumed.
/// 
/// If FORM='FORMATTED', each record is terminated with a newline (\n) character; that is, each record actually has one extra character.
/// 
/// If FORM='PRINT', the file acts like a FORM='FORMATTED' file, except for interpretation of the column-1 characters on the output (blank = single space, 0 = double space, 1 = form feed, and + = no advance).
/// 
/// If FORM='UNFORMATTED', each record is preceded and terminated with an INTEGER*4 count, making each record 8 characters longer than normal. This convention is not shared with other languages, so it is useful only for communicating between FORTRAN programs.
pub enum Access {
    Append, // Sequential and Fileopt=EOF are assumed, only write are allowed, can only be applied to disk files
    Direct, // Recl must be specified,
    Sequential, // No recl is allowed
}

pub enum Action {
    Read,
    Write,
    ReadWrite,
}

pub enum Blank {
    Null,
    Zero,
}
/// Default is formatted
pub enum Form {
    Formatted,
    Unformatted,
    Print, // makes it a print file
}

#[derive(Debug, PartialEq)]
pub enum FileOrAsterisk {
    File(String),
    Asterisk,
}
pub enum FileOpt {
    NoPad, // Formatted input only, Do not extend records with blanks if you read past the end-of-record. That is, a short record causes an abort with an error message, rather than just filling with trailing blanks and continuing.
    Buffer(i32), // Buffer size in bytes, default is 4096
    Eof, // Opens file at end-of-file, read and backspace are not allowed, write is allowed, but the file is not extended, and the file pointer is not moved.
}

#[derive(Default)]
pub struct FileMeta {
    pub ln: Option<i32>,
    pub file: Option<FileOrAsterisk>,
    pub access: Option<Access>,
    pub blank: Option<Blank>,
    pub error: Option<i32>,
    pub form: Option<Form>,
    // iostat: Option<i32>, not needed because of the Result type
    pub recl: Option<i32>, // required for direct access, length of character record for sequential access
    pub status: Option<Status>,
    pub fileopt: Option<FileOpt>,
    pub read_only: Option<bool>,
    pub action: Option<Action>,
}


/// unit must be first, then filename
#[macro_export]
macro_rules! open_file {
    (unit: $unit:expr) => {{
        let meta = FileMeta::default();
        open_file($unit, meta)
    }};
    (unit: $unit:expr, $first_key:ident: $first_value:expr $(, $key:ident: $value:expr)*) => {{
        let meta = FileMeta {
            $first_key: Some($first_value),
            $( $key: Some($value), )*
            ..Default::default()
        };
        open_file($unit, meta)
    }};
}


pub fn open_file(unit: i32, mut meta: FileMeta) -> Result<(), Error> {
    let mut map = FILE_MAP.lock().unwrap();
    meta.ln = Some(0);

    if let Some(access) = &meta.access {
        match access {
            Access::Direct => {
                if meta.recl.is_none() {
                    return Err(Error::new(ErrorKind::InvalidInput, "RECL must be specified for direct access"));
                }
            }
            Access::Sequential => {
                if meta.recl.is_some() {
                    return Err(Error::new(ErrorKind::InvalidInput, "RECL is not allowed for sequential access"));
                }
            }
            Access::Append => {
                meta.fileopt = Some(FileOpt::Eof);
                meta.action = Some(Action::Write);
                if let Some(status) = &meta.status {
                    if *status == Status::Scratch {
                        return Err(Error::new(ErrorKind::InvalidInput, "Invalid status for append"));
                    }
                }
            }
        }
    }

    if map.contains_key(&unit) {
        let existing_entry = map.get_mut(&unit).unwrap().as_mut().unwrap();

        // Reopen case: You're trying to open a file that's already open.
        if existing_entry.1.file == meta.file {
            // Only allowed to change BLANK and FORM parameters
            existing_entry.1.blank = meta.blank;
            existing_entry.1.form = meta.form;
            return Ok(());
        } else {
            // Switch Files: You're specifying a different file name, it's as if you closed the old file before the open.
            map.remove(&unit);
        }
    } else {
        // Switch Units: It's an error if you open a file that is already open but specify a different unit.
        if map.values().any(|existing_entry| existing_entry.as_ref().unwrap().1.file == meta.file) {
            return Err(Error::new(ErrorKind::InvalidInput, "File is already open with a different unit"));
        }
    }


    match &meta.status {
        Some(Status::Old) => {
            if !map.contains_key(&unit) {
                return Err(Error::new(ErrorKind::NotFound, "File not found"));
            }
        }
        Some(Status::New) => {
            if map.contains_key(&unit) {
                return Err(Error::new(ErrorKind::AlreadyExists, "File already exists"));
            }
        }
        _ => {}
    }
    let file_result: Result<File, Error> = if let Some(file_or_asterisk) = meta.file.as_ref() {
        match file_or_asterisk {
            FileOrAsterisk::File(f) => {
                match meta.status {
                    Some(Status::Old) | Some(Status::Unknown) => File::open(f),
                    Some(Status::New) => File::create(f),
                    Some(Status::Scratch) | None => tempfile::NamedTempFile::new().map(|temp| temp.into_file()),
                    Some(Status::Replace) => {
                        if !map.contains_key(&unit) {
                            return Err(Error::new(ErrorKind::NotFound, "File not found"));
                        }
                        let existing_entry = map.get_mut(&unit).unwrap().as_mut().unwrap();
                        existing_entry.0 = None;
                        File::create(f)
                    }
                }
            }
            FileOrAsterisk::Asterisk => {
                let mut buffer: String = String::new();
                stdin().read_line(&mut buffer)?;
                match meta.status {
                    Some(Status::Old) | Some(Status::Unknown) => File::open(buffer),
                    Some(Status::New) => File::create(buffer),
                    Some(Status::Scratch) | None => tempfile::NamedTempFile::new().map(|temp| temp.into_file()),
                    Some(Status::Replace) => {
                        if !map.contains_key(&unit) {
                            return Err(Error::new(ErrorKind::NotFound, "File not found"));
                        }
                        let existing_entry = map.get_mut(&unit).unwrap().as_mut().unwrap();
                        existing_entry.0 = None;
                        File::create(buffer)
                    }
                }
                
            }
        }
    } else {
        let file_name: String = format!("rust.{}", unit);
        match meta.status {
            Some(Status::Old) | Some(Status::Unknown) => File::open(&file_name),
            Some(Status::New) => File::create(&file_name),
            Some(Status::Scratch) | None => tempfile::NamedTempFile::new().map(|temp| temp.into_file()),
            Some(Status::Replace) => {
                if !map.contains_key(&unit) {
                    return Err(Error::new(ErrorKind::NotFound, "File not found"));
                }
                let existing_entry = map.get_mut(&unit).unwrap().as_mut().unwrap();
                existing_entry.0 = None;
                File::create(&file_name)
            }
        }
    };

    map.insert(unit, Ok((file_result.ok(), meta)));
    Ok(())
}

struct Read {
    unit: ReadUnit,
    fmt: FormatIdentifier,
    iostat: IOStatusSpecifier,
    rec: Option<i32>,
    end: Option<String>,
    err: Option<String>,
    iolist: Option<Vec<String>>,
    nml: Option<String>,
}

enum ReadUnit {
    ExternalUnit(i32),
    Stdin,
}

enum FormatIdentifier {
    ListDirected,                              // Corresponds to an asterisk (*)
    Label(String),                             // Label of a FORMAT statement
    IntegerVariable(String),                   // Integer variable assigned the label of a FORMAT statement
    RuntimeFormatString(String),               // Character expression specifying the format string
    RuntimeFormatArray(Vec<i32>),              // Integer array specifying the format string (nonstandard)
}
enum IOStatusSpecifier {
    IntegerVariable(i32),  // Integer variable
    IntegerArrayElement(i32, usize),  // Integer array element (value, index)
}

fn Write(unit: i32, mut meta: FileMeta) -> Result<(), Error>{
    let mut map = FILE_MAP.lock().unwrap();

    let input = match meta.file {
        Some(FileOrAsterisk::File(f)) => f,
        Some(FileOrAsterisk::Asterisk) => {
            let mut buffer: String = String::new();
            stdin().read_line(&mut buffer)?;
            buffer
        }
        None => {
            let file_name: String = format!("rust.{}", unit);
            file_name
        }
    };
    

    Ok(())
}