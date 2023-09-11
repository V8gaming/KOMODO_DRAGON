use std::{env, fs::File, io::{Error, ErrorKind, Write, BufReader, BufRead, Seek, SeekFrom, Read}, collections::HashMap, process::exit};
use tempfile::tempfile;
use ndarray::{Array1, Array3, Array2};

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

pub fn inp_read(iname: &mut String, 
    oname: &mut String, card_files: &mut HashMap<i32, Option<File>>, 
    card_indicator: &mut CardInticator, mut error_card: ErrorCard, scr: bool) {

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
    open_file(card_files,CardNumbers::Iunit.as_i32(), iname, "input", "ERROR IN INPUT FILE : ");
    *oname = format!("{}.out", iname.trim());
    oname.trim().to_string();

    card_files.insert(CardNumbers::Ounit.as_i32(), File::create(oname).ok());
    card_files.insert(CardNumbers::Umode.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uxsec.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Ugeom.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Ucase.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uesrc.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uiter.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uprnt.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uadf.as_i32(),  tempfile().ok());
    card_files.insert(CardNumbers::Ucrod.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Ubcon.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uftem.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Umtem.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Ucden.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Ucbcs.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uejct.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uther.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uxtab.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Ukern.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uextr.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uthet.as_i32(), tempfile().ok());
    card_files.insert(CardNumbers::Uoutp.as_i32(), tempfile().ok());

    //By default, card file names are the input file name
    error_card.farr = Array1::from(vec![adjustl(iname)]);

    // Echo the input to the output file
    inp_echo(card_files, scr);

    // Remove comments and write the entire input to the buffer file
    card_files.insert(CardNumbers::Buff.as_i32(), tempfile().ok());
    inp_comments(card_files.get(&CardNumbers::Iunit.as_i32()).unwrap()
    , card_files.get(&CardNumbers::Buff.as_i32()).unwrap(), '!');

    // Break the buffer file and re-write into different input card buffer
    inp_rewrite(card_files.get(&CardNumbers::Buff.as_i32()).unwrap(), 
    &mut error_card.farr, card_files.get(&CardNumbers::Bunit.as_i32()).unwrap(), card_indicator, &error_card.carr);

    // Back to the first line for all input card buffer
    backspace(card_files.get_mut(&CardNumbers::Umode.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uxsec.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ugeom.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ucase.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uesrc.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uiter.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uprnt.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uadf.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ucrod.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ubcon.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uftem.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Umtem.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ucden.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ucbcs.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uejct.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uther.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uxtab.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Ukern.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uextr.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uthet.as_i32()).unwrap());
    backspace(card_files.get_mut(&CardNumbers::Uoutp.as_i32()).unwrap());
    
    // Start reading buffer files for each input card buffer
    card_files.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"\n").ok();
    card_files.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"\n").ok();
    card_files.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(format!("{:30}", "START READING INPUT").as_bytes()).ok();
    card_files.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"\n").ok();
    card_files.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"******************************************************************************").ok();
    

    // Checking card MODE
    if card_indicator.bmode == 1 {
        inp_mode(card_files.get_mut(&CardNumbers::Umode.as_i32()).unwrap());
    } else {
        card_files.get_mut(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap()
        .write_all(format!("{:30}", format!("CARD {} DOES NOT PRESENT. THIS CARD IS MANDATORY", "%MODE")).as_bytes()).ok();
    }

    // WRITE(ounit,1021) '%MODE'
    // 1008 FORMAT (30X, 'START READING INPUT')
    // 1021 FORMAT(2X, 'CARD ', A, ' DOES NOT PRESENT. THIS CARD IS MANDATORY')
    // 1041 FORMAT(2X, 'CARD ', A, ' DOES NOT PRESENT. THIS CARD IS MANDATORY FOR ', A,' CALCULATION MODE')


}
fn open_file(hash_map: &mut HashMap<i32, Option<File>>, card_number: i32, iname: &str, file_name: &str, er_message: &str) {
    let efile: Result<File, Error> = File::open(iname);
    if efile.is_err(){
        let error = Error::new(ErrorKind::NotFound, er_message);
        println!("{}", error);
        println!("Could Not Find {} file : {}", file_name, iname);
    } else {
        hash_map.insert(card_number, efile.ok());
    }
}

pub fn adjustl(s: &str) -> String {
    let mut s = s.to_string();
    s = s.trim().to_string();
    s
}
///# Purpose:
///To rewrite the input
pub fn inp_echo(card_files: &mut HashMap<i32, Option<File>>, scr: bool) {

    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"###########################################################\n").ok();
    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"#                 KOMODO DRAGON Version: 0.1              #\n").ok();
    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"#           OPEN NUCLEAR REACTOR SIMULATOR                #\n").ok();
    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"###########################################################\n").ok();
    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"\n").ok();
    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref().unwrap().write_all(b"\n").ok();

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
    card_files.get(&CardNumbers::Ounit.as_i32())
    .unwrap().as_ref().unwrap()
    .write_all(b"###########################################################\n").ok();
    let nline = 0;
    let file = card_files.get(&CardNumbers::Iunit.as_i32()).unwrap();
    let mut reader = BufReader::new(file.as_ref().unwrap());
    let mut iline = String::new();
    match reader.read_line(&mut iline) {
        Ok(bytes_read) => {
            if bytes_read == 0 {
                // Reached EOF (end-of-file)
                card_files.get(&CardNumbers::Ounit.as_i32()).unwrap()
                .as_ref().unwrap().write_all(
                    format!(
                        " =============================INPUT DATA{} HERE===========================\n"
                    ,"ENDS").as_bytes()
                ).ok();
                card_files.get(&CardNumbers::Ounit.as_i32()).unwrap()
                .as_ref().unwrap().write_all(b"\n").ok();
                exit(-1)
            } else {
                iline.truncate(200); // Keep only first 200 characters
                card_files.get(&CardNumbers::Ounit.as_i32()).unwrap().as_ref()
                .unwrap().write_all(format!("{: >4}: {:<200}", nline, iline).as_bytes()).ok();
            }
        }
        Err(err) => {
            // Handle the error
            println!("An error occurred: {}", err);
        }
    }
    card_files.get(&CardNumbers::Ounit.as_i32()).unwrap()
    .as_ref().unwrap().seek(std::io::SeekFrom::Start(0)).ok();

}

/// Remove comments and write the entire input to the buffer file
fn inp_comments(input: &Option<File>, buffer: &Option<File>, comment_start: char) {
    let reader = BufReader::new(input.as_ref().unwrap());
    for (ln, line) in reader.lines().enumerate() {
        if line.is_err() {
            exit(-1)
        }
        let mut iline = line.unwrap();
        iline = adjustl(iline.trim()).to_string(); // Remove trailing blanks and adjust to left
        let comm = iline.find(comment_start); // Find position '!' if any ! If there is no '!' and no first 20 blank spaces (in case line is blank)
        if comm.is_none() && &iline[0..20] != "                    " {
            buffer.as_ref().unwrap().write_all(format!("x {: >5} {}\n", ln, iline).as_bytes()).ok();
        }
        // If the first character is not '!'
        if comm.is_some() && comm.unwrap() > 1 {
            iline = iline[0..comm.unwrap()].to_string(); // Take only part of input
            buffer.as_ref().unwrap().write_all(format!("x {: >5} {}\n", ln, iline).as_bytes()).ok();
        }
    }
    input.as_ref().unwrap().seek(std::io::SeekFrom::Start(0)).ok();
}

///# Purpose:
/// Read previous input buffer and rewrite and break into different input buffer
/// for particular cards. Cards identfied by %
fn inp_rewrite(buffer_file: &Option<File>, farr: &mut Array1<String>, bunit: &Option<File>, card_indicator: &mut CardInticator, carr: &Array1<String>) {
    let reader = BufReader::new(buffer_file.as_ref().unwrap());
    let mut farr = farr.clone();
    let mut local_hash_map = HashMap::new();
    
    for (ln, line_result) in reader.lines().enumerate() {
        let mut line = line_result.unwrap();
        line = line.trim().to_string();
        
        if line.contains("FILE") || line.contains("file") {
            let comm = line.find(' ').ok_or("No space found").unwrap();
            let fname = line[comm+1..].trim().to_string();
            let uarr = Array1::from(Vec::from(CardNumbers::uarr()));
            farr[GETLOC(uarr, CardNumbers::Bunit.as_i32()) as usize] = fname.clone();

            open_file(&mut local_hash_map, 997, &fname, "card", "CARD File Open Failed--status");
            let card_file = local_hash_map.get(&997).unwrap();
            let mut card_reader = BufReader::new(card_file.as_ref().unwrap());
            let cunit = tempfile();
            
            inp_comments(card_file, &cunit.ok(), '!');
            
            for (ln, iline_result) in card_reader.lines().enumerate() {
                let iline = iline_result.unwrap();
                buffer_file.as_ref().unwrap().write_all(format!("x {: >5} {}\n", ln, iline).as_bytes()).unwrap();
            }
            
        }
        let per = line.find('%'); // Find position of '%'
        if per.is_some() {
            let iline = adjustl(&line[per.unwrap()+1..]).trim().to_string();
            let card = iline.clone();
            match card.as_str() {
                "ITER" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uiter.as_i32()) as usize] = iline.clone();
                    card_indicator.biter = 1;
                },
                "PRNT" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uprnt.as_i32()) as usize] = iline.clone();
                    card_indicator.bprnt = 1;
                },
                "ADF" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uadf.as_i32()) as usize] = iline.clone();
                    card_indicator.badf = 1;
                },
                "CROD" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ucrod.as_i32()) as usize] = iline.clone();
                    card_indicator.bcrod = 1;
                },
                "EJCT" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uejct.as_i32()) as usize] = iline.clone();
                    card_indicator.bejct = 1;
                },
                "CBCS" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ucbcs.as_i32()) as usize] = iline.clone();
                    card_indicator.bcbcs = 1;
                },
                "FTEM" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uftem.as_i32()) as usize] = iline.clone();
                    card_indicator.bftem = 1;
                },
                "MTEM" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Umtem.as_i32()) as usize] = iline.clone();
                    card_indicator.bmtem = 1;
                },
                "CDEN" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ucden.as_i32()) as usize] = iline.clone();
                    card_indicator.bcden = 1;
                },
                "BCON" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ubcon.as_i32()) as usize] = iline.clone();
                    card_indicator.bbcon = 1;
                },
                "THER" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uther.as_i32()) as usize] = iline.clone();
                    card_indicator.bther = 1;
                },
                "XTAB" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uxtab.as_i32()) as usize] = iline.clone();
                    card_indicator.bxtab = 1;
                },
                "KERN" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Ukern.as_i32()) as usize] = iline.clone();
                    card_indicator.bkern = 1;
                },
                "EXTR" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uextr.as_i32()) as usize] = iline.clone();
                    card_indicator.bextr = 1;
                },
                "THET" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uthet.as_i32()) as usize] = iline.clone();
                    card_indicator.bthet = 1;
                },
                "OUTP" => {
                    farr[GETLOC(Array1::from(Vec::from(CardNumbers::uarr())), CardNumbers::Uoutp.as_i32()) as usize] = iline.clone();
                    card_indicator.boutp = 1;
                },
                _ => {
                    println!("AT LINE {} : THIS IS A WRONG INPUT CARD : {}", ln, iline);
                    exit(-1);
                }

                
            }

        }
        if per.is_none() {
            buffer_file.as_ref().unwrap().write_all(format!("x {: >5} {}\n", ln, line).as_bytes()).unwrap();
        }

    }
}

fn GETLOC(arr: Array1<CardNumbers>, elm: i32) -> i32 {
    let mut loc = 0;
    for (i,j) in arr.iter().enumerate() {
        if j.as_i32() == elm {
            loc = i as i32;
        }
    }
    loc

}
fn backspace(file: &mut Option<File>) {
    file.as_ref().unwrap().seek(SeekFrom::Start(0)).ok();
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
fn inp_mode(xbunit: &Option<File>, card_indicator: &mut CardInticator) {
    let mut line = String::new();
    let mut mode = String::new();
    let mut ln = 0;
    let mut ios = 0;
    let mut message = String::new();
    let mut mode_desc = String::new();
    let mut ind = String::new();
    let mut reader = BufReader::new(xbunit.as_ref().unwrap());
    match reader.read_line(&mut line) {
        Ok(bytes_read) => {
            if bytes_read == 0 {
                // Reached EOF (end-of-file)
                println!("ERROR IN READING MODE");
                exit(-1)
            } else {
                ind = line[0..2].to_string();
                ln = line[2..7].trim().parse::<i32>().unwrap();
                mode = line[7..].trim().to_string();
            }
        }
        Err(err) => {
            // Handle the error
            println!("An error occurred: {}", err);
        }
    }
    message = "ERROR IN READING MODE".to_string();
    er_message(xbunit, ios, ln, &message, None);
    mode = adjustl(&mode);
    match mode.as_str() {
        "FORWARD" => {
            mode_desc = "FORWARD CALCULATION".to_string();
        },
        "ADJOINT" => {
            mode_desc = "ADJOINT CALCULATION".to_string();
        },
        "FIXEDSRC" => {
            mode_desc = "FIXED SOURCE CALCULATION".to_string();
        },
        "RODEJECT" => {
            if card_indicator.bther == 0 {
                mode_desc = "ROD EJECTION CALCULATION WITHOUT T-H FEEDBACK".to_string();
            } else {
                mode_desc = "ROD EJECTION CALCULATION WITH T-H FEEDBACK".to_string();
            }
        },
        "BCSEARCH" => {
            if card_indicator.bther == 0 {
                mode_desc = "CRITICAL BORON CONCENTRATION SEARCH WITHOUT T-H FEEDBACK".to_string();
            } else {
                mode_desc = "CRITICAL BORON CONCENTRATION SEARCH WITH T-H FEEDBACK".to_string();
            }
        },
        _ => {
            println!("MODE : {} IS UNIDENTIFIED", mode);
            exit(-1);
        }
    }
    println!("CALCULATION MODE : {}", mode_desc);

    
}