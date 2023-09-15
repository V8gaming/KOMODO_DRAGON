use std::{env, fs::File, io::{Error, ErrorKind, Write, BufReader, BufRead, Seek, SeekFrom},
 collections::HashMap, process::exit};
use ndarray::{Array1, Array3, Array2};
use crate::read::{FileMeta, open_file, FileOrAsterisk, Status, Action, FILE_MAP, Access};

use crate::{open_file, backspace, rewind};

#[derive(Debug)]
pub struct Io {
    pub iname: String,
    pub oname: String,
    pub ind: String, // ind is sed to read x indicator in beginning of input buffer line. This to prevent reading next line
    pub message: String, // error message
    pub iline: String, // Input line
    pub output_options: OutputOptions,
    pub card_files: HashMap<i32, Option<File>>,
    pub card_indicator: CardInticator,
    pub error_card: ErrorCard,
    pub geometry: Geometry,
    pub opt_data: OptData,

}
#[derive(Clone)]
enum CardNumbers {
    Iunit = 100, Ounit = 101, Buff = 99,
    Umode = 111, Uxsec = 112, Ugeom = 113, Ucase = 114,
    Uesrc = 115, Uiter = 116, Uprnt = 117, Uadf  = 118,
    Ucrod = 119, Ubcon = 120, Uftem = 121, Umtem = 122,
    Ucden = 123, Ucbcs = 124, Uejct = 125, Uther = 126,
    Uxtab = 127, Ukern = 128, Uextr = 129, Uthet = 130,
    Uoutp = 131,
    Bunit = 0,

}
impl CardNumbers {
    fn as_i32( &self ) -> i32 {
        *self as i32
    }
    fn uarr() -> [CardNumbers; 21] {
        [CardNumbers::Umode, CardNumbers::Uxsec, CardNumbers::Ugeom, CardNumbers::Ucase,
        CardNumbers::Uesrc, CardNumbers::Uiter, CardNumbers::Uprnt, CardNumbers::Uadf,
        CardNumbers::Ucrod, CardNumbers::Ubcon, CardNumbers::Uftem, CardNumbers::Umtem,
        CardNumbers::Ucden, CardNumbers::Ucbcs, CardNumbers::Uejct, CardNumbers::Uther,
        CardNumbers::Uxtab, CardNumbers::Ukern, CardNumbers::Uextr, CardNumbers::Uthet,
        CardNumbers::Uoutp]
    }
}
impl Copy for CardNumbers {}

impl Default for Io {
    fn default() -> Self {
        Io { 
            iname: String::new(), 
            oname: String::new(), 
            ind: String::new(), 
            message: String::new(),  
            iline: String::new(),  
            output_options: OutputOptions::default(), 
            card_files: {
                let mut map = HashMap::new();
                let numbers: [i32; 25] = 
                [
                    99, 100, 101,
                    111, 112, 113, 114,
                    115, 116, 117, 118,
                    119, 120, 121, 122,
                    123, 124, 125, 126,
                    127, 128, 129, 130,
                    131, 0
                ];
                for i in numbers.iter() {
                    map.insert(*i, None);
                }
                map
            }, 
            card_indicator: CardInticator::default(),
            error_card: ErrorCard::default(),
            geometry: Geometry::default(),
            opt_data: OptData::default(),
        }
    }

}

#[derive(Debug)]
pub struct OutputOptions {
    pub ogeom: bool, // Geometry output print option
    pub oxsec: bool, // Macroscopic CXs output print option
    pub scr: bool, // Terminal ouput print option
}
impl Default for OutputOptions {
    fn default() -> Self {
        OutputOptions {
            ogeom: true,
            oxsec: true,
            scr: true,
        }
    }
}

#[derive(Debug, Default)]
pub struct InOutBuffFiles {
    pub iunit: Option<File>, // Input file
    pub ounit: Option<File>, // Output file
    pub buff: Option<File>, // Input buffer file (entire input)
}


#[derive(Debug,Default)]
pub struct CardInticator {
    // Card active/inactive indicator (active = 1, inactive = 0)
    pub bmode: i32, pub bxsec: i32, pub bgeom: i32, pub bcase: i32,
    pub besrc: i32, pub biter: i32, pub bprnt: i32, pub badf: i32,
    pub bcrod: i32, pub bbcon: i32, pub bftem: i32, pub bmtem: i32,
    pub bcden: i32, pub bcbcs: i32, pub bejct: i32, pub bther: i32,
    pub bxtab: i32, pub bkern: i32, pub bextr: i32, pub bthet: i32,
    pub boutp: i32,
}

#[derive(Debug)]
pub struct ErrorCard {
    // This is to notify that the error in separated card file (in case so)
    pub ncard: i32, // Number of card
    pub carr: Array1<String>, // Array of card name
    pub farr: Array1<String>, // Array of card file
    
}

impl Default for ErrorCard {
    fn default() -> Self {
        ErrorCard {
            ncard: 21,
            carr: Array1::from(vec![
                "MODE".to_string(), "XSEC".to_string(), "GEOM".to_string(), "CASE".to_string(), 
                "ESRC".to_string(), "ITER".to_string(), "PRNT".to_string(), "ADF ".to_string(),
                "CROD".to_string(), "BCON".to_string(), "FTEM".to_string(), "MTEM".to_string(),
                "CDEN".to_string(), "CBCS".to_string(), "EJCT".to_string(), "THER".to_string(),
                "XTAB".to_string(), "KERN".to_string(), "EXTR".to_string(), "THET".to_string(),
                "OUTP".to_string()]),
            farr: Array1::from(vec!["".to_string()]),
        }
    }
}

#[derive(Debug, Default)]
pub struct Geometry {
    pub np: i32, // Number of planars
    pub zpln: Array1<i32>, // Planar assignment to z direction
    pub xsize: Array1<f64>, pub ysize: Array1<f64>, pub zsize: Array1<f64>, // Assembly size for each direction
    pub plnr: Array1<Plnr>, // planar
    pub mnum: Array3<i32>,
}
#[derive(Debug, Default)]
pub struct Plnr {
    pub asm: Array2<i32>, // Material assignment into assembly
    pub node: Array2<i32>, // Material assignment into nodes
}

#[derive(Debug, Default)]
pub struct OptData {
    pub o3d: i32, // 3D output print option
    pub power: i32, // Power output print option
    pub assembly: i32, // Assembly power output print option
}

pub fn inp_read(iname: &mut String, oname: &mut String, card_indicator: &mut CardInticator, mut error_card: ErrorCard, scr: bool) -> Result<(), Error> {

    // Get the command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("NOTE : You can also write the input directly after the command");
        print!("INPUT NAME : ");
        let mut user_input = String::new();
        let stdin = std::io::stdin(); // We get `Stdin` here.
        let _ = stdin.read_line(&mut user_input);
        *iname = user_input.trim().to_string();
        // Read iname from standard input
        // TODO
    } else {
        *iname = args[1].clone();
    }

    // File operations to replace Fortran file operations
    // TODO
    io_open_file(CardNumbers::Iunit.as_i32(), iname, "input", "ERROR IN INPUT FILE : ");
    *oname = format!("{}.out", iname.trim());
    oname.trim().to_string();

    let _ = open_file!(unit: CardNumbers::Ounit.as_i32(), file: FileOrAsterisk::File(oname.to_string()), status: Status::Replace , action: Action::Write);
    let _ = open_file!(unit: CardNumbers::Umode.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uxsec.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ugeom.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ucase.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uesrc.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uiter.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uprnt.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uadf.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ucrod.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ubcon.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uftem.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Umtem.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ucden.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ucbcs.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uejct.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uther.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uxtab.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Ukern.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uextr.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uthet.as_i32(), status: Status::Scratch, action: Action::ReadWrite);
    let _ = open_file!(unit: CardNumbers::Uoutp.as_i32(), status: Status::Scratch, action: Action::ReadWrite);

    //By default, card file names are the input file name
    error_card.farr = Array1::from(vec![adjustl(iname)]);

    // Echo the input to the output file
    inp_echo(scr);

    // Remove comments and write the entire input to the buffer file
    let _ = open_file!(unit: CardNumbers::Buff.as_i32(), status: Status::Scratch, action: Action::ReadWrite);

    inp_comments(CardNumbers::Iunit.as_i32(),CardNumbers::Buff.as_i32(), '!');

    // Break the buffer file and re-write into different input card buffer
    inp_rewrite(CardNumbers::Buff.as_i32(), card_indicator, error_card.farr);

    // Back to the first line for all input card buffer
    rewind!(unit: CardNumbers::Umode.as_i32())?; rewind!(unit: CardNumbers::Uxsec.as_i32())?;
    rewind!(unit: CardNumbers::Ugeom.as_i32())?; rewind!(unit: CardNumbers::Ucase.as_i32())?;
    rewind!(unit: CardNumbers::Uesrc.as_i32())?; rewind!(unit: CardNumbers::Uiter.as_i32())?;
    rewind!(unit: CardNumbers::Uprnt.as_i32())?; rewind!(unit: CardNumbers::Uadf.as_i32())?;
    rewind!(unit: CardNumbers::Ucrod.as_i32())?; rewind!(unit: CardNumbers::Ubcon.as_i32())?;
    rewind!(unit: CardNumbers::Uftem.as_i32())?; rewind!(unit: CardNumbers::Umtem.as_i32())?;
    rewind!(unit: CardNumbers::Ucden.as_i32())?; rewind!(unit: CardNumbers::Ucbcs.as_i32())?;
    rewind!(unit: CardNumbers::Uejct.as_i32())?; rewind!(unit: CardNumbers::Uther.as_i32())?;
    rewind!(unit: CardNumbers::Uxtab.as_i32())?; rewind!(unit: CardNumbers::Ukern.as_i32())?;
    rewind!(unit: CardNumbers::Uextr.as_i32())?; rewind!(unit: CardNumbers::Uthet.as_i32())?;
    rewind!(unit: CardNumbers::Uoutp.as_i32())?;

    // Start reading buffer files for each input card buffer
    let mut map = FILE_MAP.lock().unwrap();
    map.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(b"\n").ok();
    map.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(b"\n").ok();
    map.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(format!("{:30}", "START READING INPUT").as_bytes()).ok();
    map.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(b"\n").ok();
    map.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(b"******************************************************************************").ok();

    // Checking card MODE
    if card_indicator.bmode == 1 {
        inp_mode(CardNumbers::Umode.as_i32(), card_indicator)?;
        Ok(())
    } else {
        map.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(format!("{:30}", "CARD MODE DOES NOT PRESENT. THIS CARD IS MANDATORY").as_bytes()).ok();
        exit(-1);
    }

    // WRITE(ounit,1021) '%MODE'
    // 1008 FORMAT (30X, 'START READING INPUT')
    // 1021 FORMAT(2X, 'CARD ', A, ' DOES NOT PRESENT. THIS CARD IS MANDATORY')
    // 1041 FORMAT(2X, 'CARD ', A, ' DOES NOT PRESENT. THIS CARD IS MANDATORY FOR ', A,' CALCULATION MODE')
    
}

fn io_open_file(card_number: i32, iname: &str, file_name: &str, er_message: &str) {
    let iost = open_file!(unit: card_number, file: FileOrAsterisk::File(iname.to_string()), 
    status: Status::Old, action: Action::Read);
    if iost.is_err() {
        let error = Error::new(ErrorKind::NotFound, er_message);
        println!("{}", error);
        println!("Could Not Find {} file : {}", file_name, iname);
    }
}

pub fn adjustl(s: &str) -> String {
    let mut s = s.to_string();
    s = s.trim().to_string();
    s
}
///# Purpose:
///To rewrite the input
pub fn inp_echo(scr: bool) {

    let map = FILE_MAP.lock().unwrap();
    let ounit = map.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap();
    ounit.0.as_ref().unwrap().write_all(b"###########################################################\n").ok();
    ounit.0.as_ref().unwrap().write_all(b"#                 KOMODO DRAGON Version: 0.1              #\n").ok();
    ounit.0.as_ref().unwrap().write_all(b"#           OPEN NUCLEAR REACTOR SIMULATOR                #\n").ok();
    ounit.0.as_ref().unwrap().write_all(b"###########################################################\n").ok();
    ounit.0.as_ref().unwrap().write_all(b"\n").ok();
    ounit.0.as_ref().unwrap().write_all(b"\n").ok();

    if scr {
        println!();
        println!();
        println!("###########################################################");
        println!("#                 KOMODO DRAGON Version: 0.1              #");
        println!("#           OPEN NUCLEAR REACTOR SIMULATOR                #");
        println!("###########################################################");

    }
    #[cfg(feature = "git_info")]
    {
        println!("GIT COMMIT SHA    : {}", env!("GIT_COMMIT_HASH"));
        println!("GIT COMMIT DATE   : {}", env!("GIT_DATE"));
        println!("GIT COMMIT BRANCH : {}", env!("GIT_BRANCH"));
    }
    ounit.0.as_ref().unwrap().write_all(b"###########################################################\n").ok();
    let nline = 0;
    let iunit = map.get(&CardNumbers::Iunit.as_i32()).unwrap().as_ref().unwrap();
    let mut reader = BufReader::new(iunit.0.as_ref().unwrap());
    let mut iline = String::new();
    match reader.read_line(&mut iline) {
        Ok(bytes_read) => {
            if bytes_read == 0 {
                // Reached EOF (end-of-file)
                ounit.0.as_ref().unwrap().write_all(
                    format!(
                        " =============================INPUT DATA{} HERE===========================\n"
                    ,"ENDS").as_bytes()
                ).ok();
                ounit.0.as_ref().unwrap().write_all(b"\n").ok();
                exit(-1)
            } else {
                iline.truncate(200); // Keep only first 200 characters
                ounit.0.as_ref().unwrap().write_all(format!("{: >4}: {:<200}", nline, iline).as_bytes()).ok();
            }
        }
        Err(err) => {
            // Handle the error
            println!("An error occurred: {}", err);
        }
    }
    let _ = backspace!(unit: CardNumbers::Ounit.as_i32());


}

/// Remove comments and write the entire input to the buffer file
fn inp_comments(input: i32, buffer: i32, comment_start: char) {
    let _ = open_file!(unit: buffer, status: Status::Scratch, action: Action::ReadWrite);
    let file_map = FILE_MAP.lock().unwrap();
    let mut input_file = file_map.get(&input).unwrap().as_ref().unwrap().0.as_ref().unwrap();
    let reader = BufReader::new(input_file);
    let mut buffer = file_map.get(&buffer).unwrap().as_ref().unwrap().0.as_ref().unwrap();
    for (ln, line) in reader.lines().enumerate() {
        if line.is_err() {
            exit(-1)
        }
        let mut iline = line.unwrap();
        iline = adjustl(iline.trim()).to_string(); // Remove trailing blanks and adjust to left
        let comm = iline.find(comment_start); // Find position '!' if any ! If there is no '!' and no first 20 blank spaces (in case line is blank)
        if comm.is_none() && &iline[0..20] != "                    " {
            buffer.write_all(format!("x {: >5} {}\n", ln, iline).as_bytes()).ok();
        }
        // If the first character is not '!'
        if let Some(comm_val) = comm {
            if comm_val > 1 {
                iline = iline[0..comm_val].to_string(); // Take only part of input
                buffer.write_all(format!("x {: >5} {}\n", ln, iline).as_bytes()).ok();
            }
        }
    }
    
    input_file.seek(std::io::SeekFrom::Start(0)).ok();
}

///# Purpose:
/// Read previous input buffer and rewrite and break into different input buffer
/// for particular cards. Cards identfied by %
fn inp_rewrite(buffer_file: i32, card_indicator: &mut CardInticator, mut farr: Array1<String>) {
    let file_map = FILE_MAP.lock().unwrap();
    let reader = BufReader::new(file_map.get(&buffer_file).unwrap().as_ref()
    .unwrap().0.as_ref().unwrap());
    let cunit = 996;
    let xunit = 997;
    for (ln, line_result) in reader.lines().enumerate() {
        let mut line = line_result.unwrap();
        line = line.trim().to_string();
        
        if line.contains("FILE") || line.contains("file") {
            let comm = line.find(' ').ok_or("No space found").unwrap();
            let fname = line[comm+1..].trim().to_string();
            let uarr = Array1::from(Vec::from(CardNumbers::uarr()));
            farr[getloc(uarr, CardNumbers::Bunit.as_i32()) as usize] = fname.clone();

            io_open_file( xunit, &fname, "card", "CARD File Open Failed--status");
            inp_comments(xunit, cunit, '!');
            
            let card_reader = BufReader::new(file_map.get(&cunit).unwrap().as_ref().unwrap().0.as_ref().unwrap());
            for (ln, iline_result) in card_reader.lines().enumerate() {
                let iline = iline_result.unwrap();
                file_map.get(&cunit).unwrap().as_ref().unwrap().0.as_ref().unwrap().write_all(format!("x {: >5} {}\n", ln, iline).as_bytes()).unwrap();
            }
            
        }
        let per = line.find('%'); // Find position of '%'
        if let Some(per_val) = per {
            let iline = adjustl(&line[per_val + 1..]).trim().to_string();
            let card = iline.clone();
            match card.as_str() {
                "ITER" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uiter.as_i32()) as usize] = iline.clone();
                    card_indicator.biter = 1;
                },
                "PRNT" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uprnt.as_i32()) as usize] = iline.clone();
                    card_indicator.bprnt = 1;
                },
                "ADF" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uadf.as_i32()) as usize] = iline.clone();
                    card_indicator.badf = 1;
                },
                "CROD" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ucrod.as_i32()) as usize] = iline.clone();
                    card_indicator.bcrod = 1;
                },
                "EJCT" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uejct.as_i32()) as usize] = iline.clone();
                    card_indicator.bejct = 1;
                },
                "CBCS" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ucbcs.as_i32()) as usize] = iline.clone();
                    card_indicator.bcbcs = 1;
                },
                "FTEM" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uftem.as_i32()) as usize] = iline.clone();
                    card_indicator.bftem = 1;
                },
                "MTEM" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Umtem.as_i32()) as usize] = iline.clone();
                    card_indicator.bmtem = 1;
                },
                "CDEN" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ucden.as_i32()) as usize] = iline.clone();
                    card_indicator.bcden = 1;
                },
                "BCON" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ubcon.as_i32()) as usize] = iline.clone();
                    card_indicator.bbcon = 1;
                },
                "THER" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uther.as_i32()) as usize] = iline.clone();
                    card_indicator.bther = 1;
                },
                "XTAB" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uxtab.as_i32()) as usize] = iline.clone();
                    card_indicator.bxtab = 1;
                },
                "KERN" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ukern.as_i32()) as usize] = iline.clone();
                    card_indicator.bkern = 1;
                },
                "EXTR" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uextr.as_i32()) as usize] = iline.clone();
                    card_indicator.bextr = 1;
                },
                "THET" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uthet.as_i32()) as usize] = iline.clone();
                    card_indicator.bthet = 1;
                },
                "OUTP" => {
                    farr[getloc(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uoutp.as_i32()) as usize] = iline.clone();
                    card_indicator.boutp = 1;
                },
                _ => {
                    println!("AT LINE {} : THIS IS A WRONG INPUT CARD : {}", ln, iline);
                    exit(-1);
                }

                
            }

        }
        if per.is_none() {
            FILE_MAP.lock().unwrap().get(&CardNumbers::Umode.as_i32()).unwrap()
            .as_ref().unwrap().0.as_ref().unwrap().write_all(format!("x {: >5} {}\n", ln, line)
            .as_bytes()).unwrap();
        }

    }
}

fn getloc(arr: Array1<CardNumbers>, elm: i32) -> i32 {
    let mut loc = 0;
    for (i,j) in arr.iter().enumerate() {
        if j.as_i32() == elm {
            loc = i as i32;
        }
    }
    loc

}
///The BACKSPACE statement positions the specified file to just before the preceding record.
/// BACKSPACE in a terminal file has no effect.
///u must be connected for sequential access. Execution of a BACKSPACE statement on a direct-access file is not defined in the FORTRAN 77 Standard, and is unpredictable. We do not recommend using a BACKSPACE statement on a direct-access file or an append access file.
///Execution of the BACKSPACE statement modifies the file position, as follows:
/// 
///| Prior to Execution               | After Execution                      |
///|----------------------------------|--------------------------------------|
///| Beginning of the file            | Remains at the beginning of the file |
///| Beyond the endfile record        | before the endfile record            |
///| Beginning of the previous record | Beginning of the previous record     |
/// 
fn backspace(unit: i32, error: Option<String>) -> Result<(), std::io::Error> {
    let mut file_map = FILE_MAP.lock().unwrap();
    let file_entry_result = file_map.get_mut(&unit).ok_or(std::io::Error::new(std::io::ErrorKind::NotFound, "File unit not found"));
    
    let file_entry = match file_entry_result {
        Ok(entry) => entry,
        Err(e) => return Err(e)
    };

    let (file_opt, meta) = file_entry.as_mut().unwrap();
    if meta.access.is_some() && meta.access != Some(Access::Sequential) {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            error.unwrap_or_else(|| {
                "BACKSPACE statement on a direct-access file is not defined in the FORTRAN 77 Standard, and is unpredictable. We do not recommend using a BACKSPACE statement on a direct-access file or an append access file.".to_string()
            }),
        ));
    }

    let line_number = meta.ln.ok_or(std::io::Error::new(std::io::ErrorKind::Other, "Line number not set"))?;
    
    if line_number == 0 {
        return Ok(());
    }

    let file = file_opt.as_mut().ok_or(std::io::Error::new(std::io::ErrorKind::Other, "File not set"))?;
    let end = file.seek(std::io::SeekFrom::End(0))? as i32;

    if line_number > end {
        meta.ln = Some(end);
    }

    let current_pos = file.stream_position()? as i32;
    
    if line_number < current_pos {
        meta.ln = Some(file.seek(std::io::SeekFrom::Current(-1))? as i32);
    }

    Ok(())
}

#[macro_export]
macro_rules! backspace {
    (unit: $unit:expr) => {
        backspace($unit, None)
    };
    (unit: $unit:expr, error: $error:expr) => {
        backspace($unit, Some($error))
    };
        
}

fn rewind(unit: i32, error: Option<String>) -> Result<(), Error> {
    let mut file_map = FILE_MAP.lock().unwrap();
    let file_entry_result = file_map.get_mut(&unit).ok_or(std::io::Error::new(std::io::ErrorKind::NotFound, "File unit not found"));
    
    let file_entry = match file_entry_result {
        Ok(entry) => entry,
        Err(e) => return Err(e)
    };
    let (file_opt, meta) = file_entry.as_mut().unwrap();

    match meta.access {
        Some(Access::Sequential) => {
            meta.ln = Some(0);
            let file = file_opt.as_mut().ok_or(Error::new(ErrorKind::Other, "File not set"))?;
            file.seek(SeekFrom::Start(0))?;
            Ok(())
        },
        Some(Access::Append) => {
            meta.ln = Some(0);
            let file = file_opt.as_mut().ok_or(Error::new(ErrorKind::Other, "File not set"))?;
            file.seek(SeekFrom::Start(0))?;
            Ok(())
        },
        Some(Access::Direct) => {
            if error.is_some() {
                Err(Error::new(ErrorKind::Other, error.unwrap()))
            }
            else {
                Err(Error::new(ErrorKind::Other, "REWIND statement on a direct-access file is not defined in the FORTRAN 77 Standard, and is unpredictable. We do not recommend using a REWIND statement on a direct-access file or an append access file."))
            }
        }
        None => {
            Err(Error::new(ErrorKind::Other, "Access not set"))
        }
    }

}
#[macro_export]
macro_rules! rewind {
    (unit: $unit:expr) => {
        rewind($unit, None)
    };
    (unit: $unit:expr, error: $error:expr) => {
        rewind($unit, Some($error))
    };
        
}

/// # Purpose:
/// To read case mode in input
/// translated from fortran subroutine inp_mode
/// ```fortran 
/// SUBROUTINE inp_mode (xbunit)
/// !
/// ! Purpose:
/// !    To read case mode in input
/// !
/// 
/// USE sdata, ONLY: mode
/// 
/// IMPLICIT NONE
/// 
/// INTEGER, INTENT(IN) :: xbunit
/// 
/// INTEGER :: ln   !Line number
/// INTEGER :: ios  ! IOSTAT status
/// CHARACTER(LEN=60) :: mode_desc
/// 
/// READ(xbunit, '(A2, I5,A100)', IOSTAT=ios) ind, ln, mode
/// message = ' error in reading MODE'
/// CALL er_message(ounit, ios, ln, message, buf=xbunit)
/// 
/// 
/// mode = TRIM(ADJUSTL(mode))  ! ADJUSTL = MOVE PRECEDING BLANK TO TRAILING
/// 
/// SELECT CASE(mode)
/// CASE('FORWARD')
///     mode_desc = TRIM(ADJUSTL('FORWARD CALCULATION'))
/// CASE('ADJOINT')
///   mode_desc = TRIM(ADJUSTL('ADJOINT CALCULATION'))
/// CASE('FIXEDSRC')
///   mode_desc = TRIM(ADJUSTL('FIXED SOURCE CALCULATION'))
/// CASE('RODEJECT')
///   if (bther == 0) then
///     mode_desc = TRIM(ADJUSTL('ROD EJECTION CALCULATION WITHOUT T-H FEEDBACK'))
///   else
///     mode_desc = TRIM(ADJUSTL('ROD EJECTION CALCULATION WITH T-H FEEDBACK'))
///   end if
/// CASE('BCSEARCH')
///   if (bther == 0) then
///     mode_desc = TRIM(ADJUSTL('CRITICAL BORON CONCENTRATION SEARCH' &
///     // ' WITHOUT T-H FEEDBACK'))
///   else
///     mode_desc = TRIM(ADJUSTL('CRITICAL BORON CONCENTRATION SEARCH' &
///     // ' WITH T-H FEEDBACK'))
///   end if
/// CASE DEFAULT
///   WRITE(ounit,1032) mode
///   WRITE(*,1032) mode
///   STOP
/// END SELECT
/// 
/// WRITE(ounit,1031) mode_desc
/// WRITE(ounit,*)
/// if (scr) then
///   WRITE(*,1031) mode_desc
///   WRITE(*,*)
/// end if
/// 
/// 1031 FORMAT(2X, 'CALCULATION MODE : ', A60)
/// 1032 FORMAT(2X, 'MODE : ', A10, ' IS UNIDENTIFIED')
/// 
/// END SUBROUTINE inp_mode
/// ```
fn inp_mode(xbunit: i32, card_indicator: &mut CardInticator) -> Result<(), Error> {
    let map = FILE_MAP.lock().unwrap();
    let mut reader = BufReader::new(map.get(&xbunit).unwrap().as_ref().
    unwrap().0.as_ref().unwrap());
    for (ln, line) in reader.lines().enumerate() {
        let mut iline = line?;
        iline = iline.trim().to_string();  
    }
    Ok(())
}

/// ```fortran
/// SUBROUTINE er_message (funit, iost, ln, mess, xtab, buf)
/// !
/// ! Purpose:
/// !    To provide error message
/// !
///
///IMPLICIT NONE
///
///INTEGER, INTENT(IN) :: funit, iost, ln
///CHARACTER(LEN=*), INTENT(IN) :: mess
///INTEGER, OPTIONAL, INTENT(IN) :: xtab, buf
///
///IF (iost < 0) THEN
///  WRITE(funit,*)
///  WRITE(*,*)
///  WRITE(*,*)''//achar(27)//'[31m OOPS! WE FOUND AN ERROR.'//achar(27)//'[0m'
///
///  IF (PRESENT(xtab)) THEN
///    WRITE(funit, 1014) ln, xtab
///  ELSE
///    WRITE(funit, 1006) carr(getloc(uarr,buf))
///    WRITE(funit, 1013) ln, farr(getloc(uarr,buf))
///  END IF
///  WRITE(funit,*) mess
///  IF (PRESENT(xtab)) THEN
///    WRITE(*, 1014) ln, xtab
///  ELSE
///    WRITE(*, 1006) carr(getloc(uarr,buf))
///    WRITE(*, 1013) ln, farr(getloc(uarr,buf))
///  END IF
///  WRITE(*,*) mess
///  1013 FORMAT(2x, 'THIS LINE NEEDS MORE INPUT DATA. LINE', I4, &
///  ' IN FILE : ', A100)
///  1014 FORMAT(2x, 'ERROR: LINE', I4, &
///  'IN XTAB FILE FOR MATERIAL NUMBER' , I4, '. IT NEEDS MORE DATA')
///  STOP
///END IF
///
///IF (iost > 0) THEN
///  WRITE(funit,*)
///  WRITE(*,*)
///  WRITE(*,*)''//achar(27)//'[31m OOPS! WE FOUND AN ERROR.'//achar(27)//'[0m'
///
///  IF (PRESENT(xtab)) THEN
///    WRITE(funit, 1005) ln, xtab
///  ELSE
///    WRITE(funit, 1006) carr(getloc(uarr,buf))
///    WRITE(funit, 1004) ln, farr(getloc(uarr,buf))
///  END IF
///  WRITE(funit,*) mess
///  IF (PRESENT(xtab)) THEN
///    WRITE(*, 1005) ln, xtab
///  ELSE
///    WRITE(*, 1006) carr(getloc(uarr,buf))
///    WRITE(*, 1004) ln, farr(getloc(uarr,buf))
///  END IF
///  WRITE(*,*) mess
///  1006 FORMAT(2X, 'ERROR: THERE IS AN ERROR IN CARD %', A4)
///  1004 FORMAT(2X, 'PLEASE CHECK LINE NUMBER', I4, ' IN FILE : ', A100)
///  1005 FORMAT(2X, 'ERROR: PLEASE CHECK LINE NUMBER', I4, &
///  ' IN XTAB FILE FOR MATERIAL NUMBER ', I4)
///  STOP
///END IF
///
///END SUBROUTINE er_message
/// ```
fn er_message(funit: i32, iost: i32, ln: i32, mess: String, xtab: Option<i32>, buf: Option<i32> ) {

    
}