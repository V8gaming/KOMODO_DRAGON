use std::io::Error;

use komodo_dragon::control::{fixedsrc, adjoint, cbsearch, cbsearcht, forward};
use komodo_dragon::data::{SData, Timing};
use komodo_dragon::io::{Io, inp_read};
use komodo_dragon::trans::{rod_eject, rod_eject_th};
use komodo_dragon::read::STATEMENT_MAP;
fn main() -> Result<(), Error> {
    STATEMENT_MAP.lock().unwrap().insert(1123, ("  CPU time breakdown in seconds").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1124, ("    Input reading time   : {:10.4}  ({:5.1}%)").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1125, ("    XSEC processing time : {:10.4}  ({:5.1}%)").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1126, ("    CMFD time            : {:10.4}  ({:5.1}%)").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1127, ("    Nodal update time    : {:10.4}  ({:5.1}%)").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1128, ("    T-H time             : {:10.4}  ({:5.1}%)").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1129, ("    ------------------------------------------").to_string());
    STATEMENT_MAP.lock().unwrap().insert(1130, ("    Total time           : {:10.4}").to_string());
    let mut sdata: SData = SData::default();
    let mut io: Io = Io::default();
    // Read input
    let st = Timing::get_time();
    inp_read(&mut io.iname, &mut io.oname, 
        &mut io.card_indicator, io.error_card, io.output_options.scr)?;
    let fn_time = Timing::get_time();
    sdata.timing.inp_time = fn_time - st;
    
    if io.output_options.scr {
        println!("\nreading input ... done");
    }

    match sdata.mode.as_str() {
        "FIXEDSRC" => fixedsrc()?,
        "ADJOINT" => adjoint()?,
        "RODEJECT" => {
            if io.card_indicator.bther == 0 {
                rod_eject()?;
            } else {
                rod_eject_th()?;
            }
        },
        "BCSEARCH" => {
            if io.card_indicator.bther == 0 {
                cbsearch()?;
            } else {
                cbsearcht()?;
            }
        },
        _ => forward()?
    }

    if sdata.transient_parameters.tranw {
        // Placeholder for output unit (ounit) in Rust
        println!("\nWARNING: ONE OR MORE OUTER ITERATIONS DID NOT CONVERGE. YOU MAY NEED TO REDUCE TIME STEP");
    }
    let fdm_time = sdata.timing.fdm_time;
    let th_time = sdata.timing.th_time;
    let nod_time = sdata.timing.nod_time;
    let xs_time = sdata.timing.xs_time;
    let inp_time = sdata.timing.inp_time;

    let tot_time = fdm_time + th_time + nod_time + xs_time + inp_time;

    // WRITE operations
    println!(); 
    println!(); 
    println!("  CPU time breakdown in seconds");
    println!("    Input reading time   : {:10.4}  ({:5.1}%)", inp_time, inp_time / tot_time * 100.0);
    println!("    XSEC processing time : {:10.4}  ({:5.1}%)", xs_time, xs_time / tot_time * 100.0);
    println!("    CMFD time            : {:10.4}  ({:5.1}%)", fdm_time, fdm_time / tot_time * 100.0);
    println!("    Nodal update time    : {:10.4}  ({:5.1}%)", nod_time, nod_time / tot_time * 100.0);
    println!("    T-H time             : {:10.4}  ({:5.1}%)", th_time, th_time / tot_time * 100.0);
    println!("    ------------------------------------------");
    println!("    Total time           : {:10.4}", tot_time);

    // Conditional output to console
    if io.output_options.scr {
        println!(); 
        println!(); 
        println!("  CPU time breakdown in seconds");
        println!("    Input reading time   : {:10.4}  ({:5.1}%)", inp_time, inp_time / tot_time * 100.0);
        println!("    XSEC processing time : {:10.4}  ({:5.1}%)", xs_time, xs_time / tot_time * 100.0);
        println!("    CMFD time            : {:10.4}  ({:5.1}%)", fdm_time, fdm_time / tot_time * 100.0);
        println!("    Nodal update time    : {:10.4}  ({:5.1}%)", nod_time, nod_time / tot_time * 100.0);
        println!("    T-H time             : {:10.4}  ({:5.1}%)", th_time, th_time / tot_time * 100.0);
        println!("    ------------------------------------------");
        println!("    Total time           : {:10.4}", tot_time);
    }

    
    println!("\nKOMODO EXIT NORMALLY");
    Ok(())
}
