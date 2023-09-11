use std::{default, f64::consts::PI, time::Instant};

use ndarray::{Array1,Array2, Array3, Array4};

#[derive(Debug, Default)]
pub struct XSecNode {
    pub sigtr: Array2<f64>,
    pub siga:  Array2<f64>,
    pub nuf:   Array2<f64>,
    pub sigf:  Array2<f64>,
    pub chi:   Array2<f64>,
    pub sigs:  Array3<f64>,
    pub dc:    Array3<f64>,
    pub d:     Array2<f64>,
    pub sigr:  Array2<f64>,
}

#[derive(Debug, Default)]
pub struct XSecMaterial {
    pub xsigtr:Array2<f64>,
    pub xsiga: Array2<f64>,
    pub xnuf:  Array2<f64>,
    pub xsigf: Array2<f64>,
    pub xd:    Array2<f64>,
    pub xsigr: Array2<f64>,
    pub xsigs: Array3<f64>,
    pub ccnuf: bool,
    pub ccsigf: bool,
}

#[derive(Debug, Default)]
pub struct Geometry {
    pub nx: i32,
    pub ny: i32,
    pub nz: i32,
    pub nxx: i32,
    pub nyy: i32,
    pub nzz: i32,
    pub nnod: i32,
    pub ix: Vec<i32>,
    pub iy: Vec<i32>,
    pub iz: Vec<i32>,
    pub xyz: Array3<i32>,
    pub xdiv: Vec<i32>,
    pub ydiv: Vec<i32>,
    pub zdiv: Vec<i32>,
    pub xdel: Vec<f64>,
    pub ydel: Vec<f64>,
    pub zdel: Vec<f64>,
    pub vdel: Vec<f64>,
    pub xwest: i32,
    pub xeast: i32,
    pub ysouth: i32,
    pub ynorth: i32,
    pub zbott: i32,
    pub ztop: i32,
    pub mat: Vec<i32>,
}

#[derive(Debug, Default)]
pub struct FdmMatr {
    pub elmn: Array1<f64>,     
    pub row: Array1<i32>,      
    pub col: Array1<i32>,     
}

pub type FdmMatrixArray = Array1<FdmMatr>;

#[derive(Debug, Default)]
pub struct FdmIndex {
    pub row: Array1<i32>,
    pub col: Array1<i32>,
}

#[derive(Debug, Default)]
pub struct FdmMatrix {
    pub a: FdmMatrixArray,
    pub index: FdmIndex,
}

// Keff, flux and currents
#[derive(Debug, Default)]
pub struct KeffFluxCurrents {
    pub ke: f64, 
    pub nod: Array2<NodeData>,
    pub f0: Array2<f64>, // current flux
    pub ft: Array2<f64>, // previous flux
    pub fs0: Array1<f64>, // fission source
    pub fst: Array1<f64>, // fission source
    pub c0: Array2<f64>, // neutron precusor density
    pub s0: Array2<f64>, // neutron precusor density
    pub ystag: StaggeredArray, 
    pub xstag: StaggeredArray, 
}
#[derive(Debug, Default)]
pub struct NodeData {
    pub df: Array1<f64>, // FDM nodal coupling coefficients (X+,X-,Y+, Y-, Z+, Z-)
    pub dn: Array1<f64>, // Corrected (higher order) nodal coupling coefficients (X+,X-,Y+, Y-, Z+, Z-)
}

#[derive(Debug, Default)]
pub struct Staggered {
    //imax and imin along x and y direction for staggered nodes
    pub smax: f64, 
    pub smin: f64,
}

pub type StaggeredArray = Array1<Staggered>;

#[derive(Debug, Default)]
pub struct ExtraSources {
    pub exsrc: Array2<f64>,
    pub powtot: f64
}

#[derive(Debug)]
pub struct IterationControl {
    pub ferc: f64,    // Flux Error Criteria
    pub serc: f64,    // Fission source Error CRITERIA
    pub fer: f64,     // Flux error in BCSEARCH calcs.
    pub ser: f64,     // Fission source error in BCSEARCH calcs.
    pub nin: i32,     // Maximum inner iteration
    pub nout: i32,    // Maximum outer iteration
    pub nac: i32,     // Fission source extrapolation interval
    pub th_niter: i32,// Maximum number of thermal-hydraulics iteration
    pub nth: i32,     // Maximum number of outer iterations per thermal-hydraulics iteration
    pub nupd: i32,    // Nodal update interval

}

impl default::Default for IterationControl {
    fn default() -> IterationControl {
        IterationControl {    
            ferc: 1e-5,     
            serc: 1e-5,   
            fer: 1e-5,      
            ser: 1e-5,      
            nin: 2,         
            nout: 500,      
            nac: 5,         
            th_niter: 30,   
            nth: 20,        
            nupd: 1,        
            }
        }      
}
#[derive(Debug)]
pub struct OutputPrintOption {
    pub aprad: bool,
    pub apaxi: bool,
    pub afrad: bool,
}
impl Default for OutputPrintOption {
    fn default() -> OutputPrintOption {
        OutputPrintOption {
            aprad: true,
            apaxi: true,
            afrad: true,
        }
    }
}

#[derive(Debug, Default)]
pub struct FuelTemperature {
    pub ftem: Array1<f64>,       // Fuel temperature in Kelvin for each nodes
    pub rftem: f64,      // Fuel temperature Reference in Kelvin
    pub fsigtr: Array2<f64>, // XSEC changes per fuel temp changes
    pub fsiga: Array2<f64>,
    pub fnuf: Array2<f64>,
    pub fsigf: Array2<f64>,   
    pub fsigs: Array3<f64>,
}

#[derive(Debug, Default)]
pub struct ModeratorTemperature {
    pub mtem: Array1<f64>,       // Moderator temperature in Kelvin for each nodes
    pub rftem: f64,      // Moderator temperature Reference in Kelvin
    pub msigtr: Array2<f64>, // XSEC changes per moderator temp changes
    pub msiga: Array2<f64>,
    pub mnuf: Array2<f64>,
    pub msigf: Array2<f64>,   
    pub msigs: Array3<f64>,
}

#[derive(Debug, Default)]
pub struct CoolentDensity {
    pub cden: Array1<f64>,       // Coolent density in g/cm3 for each nodes
    pub rcden: f64,      // Coolent density Reference in g/cm3
    pub lsigtr: Array2<f64>, // XSEC changes per coolent density changes
    pub lsiga: Array2<f64>,
    pub lnuf: Array2<f64>,
    pub lsigf: Array2<f64>,   
    pub lsigs: Array3<f64>,

}

#[derive(Debug, Default)]
pub struct CrodChanges {
    pub nb: i32, // Number of CR banks
    pub bpos: Array1<f64>, // CR bank position
    pub fbpos: Array1<f64>, // Final CR bank position
    pub dsigtr: Array2<f64>, // XSEC incerement or decrement due to CR insertion
    pub dsiga: Array2<f64>,
    pub dnuf: Array2<f64>,
    pub dsigf: Array2<f64>,
    pub dsigs: Array3<f64>,
    pub ddc: Array3<f64>, // increment or decreent for ADF
    pub tmove: Array1<f64>, // Time when CR bank starts moving
    pub bspeed: Array1<f64>, // CR bank movement speed
    pub mdir: Array1<i32>, // To indicate CR movement direction (0=do not move, 1=down, 2 = up)
    pub nstep: f64, // Number of steps
    pub coreh: f64, // Core Height
    pub fbmap: Array2<i32>, // Radial control rod bank map (node wise)
    pub pos0: f64, // Zero step position and step size
    pub ssize: f64,
}
#[derive(Debug, Default)]
pub struct BoronConcentration {
    pub bcon: f64, // Boron concentration in ppm
    pub rbcon: f64, // Boron concentration in ppm Reference
    pub csigtr: Array2<f64>, // XSEC changes due to boron concentration
    pub csiga: Array2<f64>,
    pub cnuf: Array2<f64>,
    pub csigf: Array2<f64>,
    pub csigs: Array3<f64>, // Used only for CBCS card
}

#[derive(Debug)]
pub struct TransientParameters {
    pub nf: i32, // Number of delayed neutron precusor family
    pub sth: f64, // Small theta and big theta for transient using theta method
    pub bth: f64,
    pub ibeta: Array1<f64>, // beta (delayed neutron fraction) and precusor decay constant
    pub lamb: Array1<f64>,
    pub tbeta: Array1<f64>, // total beta
    pub ctbeta: f64, // Core averaged
    pub velo: Array1<f64>, // Neutron velocity
    pub ttot: f64, // TOTAL SIMULATION TIME
    pub tstep1: f64, // FIRST TIME STEP
    pub tstep2: f64, // SECOND TIME STEP
    pub tdiv: f64, // WHEN SECOND TIME STEP APPLY
    pub omeg: Array2<f64>, // Exponential transformation constant
    pub sigrp: Array2<f64>, // Initial removal cross sections before added by parameters required for transient
    pub l: Array2<f64>, // Total leakages for node n and group g
    pub dfis: Array1<f64>,
    pub tranw: bool, // To activate unconverged  outer iteration warning

}
impl default::Default for TransientParameters {
    fn default() -> TransientParameters {
        TransientParameters {
            nf: 6,
            sth: 1.0,
            bth: 0.0,
            ibeta: Array1::zeros(6),
            lamb: Array1::zeros(6),
            tbeta: Array1::zeros(6),
            ctbeta: 0.0,
            velo: Array1::zeros(6),
            ttot: 0.0,
            tstep1: 0.0,
            tstep2: 0.0,
            tdiv: 0.0,
            omeg: Array2::zeros((6,6)),
            sigrp: Array2::zeros((6,6)),
            l: Array2::zeros((6,6)),
            dfis: Array1::zeros(6),
            tranw: false,
        }
    }
}
#[derive(Debug)]
pub struct ThermalHydraulicsParameters {
    pub pow: f64, // Reactor power for given geometry (watt)
    pub ppow: f64, // Reactor percent power in percent
    pub tpow: f64, // Total reactor power
    pub npow: Array1<f64>, // nodes power (watt)
    pub tin: f64, // coolant inlet temperature (kelvin)
    pub cflow: f64, // Sub-channel mass flow rate (kg/s)
    pub rf: f64, pub tg: f64, pub tc: f64, pub ppitch: f64, // Fuel meat radius, gap thickness, clad thickness, and pin picth (m)
    pub rg: f64, pub rc: f64, // Outer radius of gap and cladding
    pub dia: f64, pub dh: f64, pub farea: f64, // Pi diameter, Hydraulic diameter (m) and sub-channel area (m2)
    pub cf: f64, // heat fraction deposited into coolant
    pub node_nf: Array2<f64>, // Number of fuel pin per node
    pub nm: i32, // number of Fuel meat mesh
    pub nt: i32, // Number Total mesh (+2 mesh for gap and clad)
    pub tfm: Array2<f64>, // Fuel pin mesh temperature for each nodes
    pub rdel: Array1<f64>, // mesh delta
    pub rpos: Array1<f64>, // mesh position
    pub th_err: f64, // Doppler error
    pub ent: Array1<f64>, // Coolant Enthalpy (J/Kg)
    pub heatf: Array1<f64>, // Heat flux (W/m2
    pub frate: Array1<f64>, // coolant mass flow rate
    pub thunit: i32, // Unit number to open steam table file
    pub pi: f64,

}
impl default::Default for ThermalHydraulicsParameters {
    fn default() -> ThermalHydraulicsParameters {
        ThermalHydraulicsParameters {
            nm: 10,
            nt: 2, // Number Total mesh (+2 mesh for gap and clad)
            thunit: 300,
            pi: PI,
            pow: 0.0,
            ppow: 0.0,
            tpow: 0.0,
            npow: Array1::zeros(2),
            tin: 0.0,
            cflow: 0.0,
            rf: 0.0, tg: 0.0, tc: 0.0, ppitch: 0.0,
            rg: 0.0, rc: 0.0,
            dia: 0.0, dh: 0.0, farea: 0.0,
            cf: 0.0,
            node_nf: Array2::zeros((2,2)),
            tfm: Array2::zeros((2,2)),
            rdel: Array1::zeros(2),
            rpos: Array1::zeros(2),
            th_err: 0.0,
            ent: Array1::zeros(2),
            heatf: Array1::zeros(2),
            frate: Array1::zeros(2),

        }
    }
}

#[derive(Debug)]
pub struct SteamTableData {
    pub ntem: i32, // Number of temperature in steam table
    pub stab: Array2<f64>, // Steam table matrixs
}
impl default::Default for SteamTableData {
    fn default() -> SteamTableData {
        SteamTableData {
            ntem: 9,
            stab: Array2::zeros((9,6)),
        }
    }
}

#[derive(Debug, Default)]
pub struct XBranch {
    // Data type for branch xsec data used if XTAB file present
    pub sigtr: Array1<f64>, //  XSEC
    pub siga: Array1<f64>,
    pub nuf: Array1<f64>,
    pub sigf: Array1<f64>,
    pub sigs: Array2<f64>,
    pub dc: Array2<f64>, // ASSEMBLY DISCONTINUITY FACTOR
}

#[derive(Debug, Default)]
pub struct MBranch {
    pub nd: i32, pub nb: i32, pub nf: i32, pub nm: i32, // BRANCH PARAMETER DIMENSION (Coolant dens., boron conc., fuel and moderator temp.)
    pub pd: Array1<f64>, pub pb: Array1<f64>, pub pf: Array1<f64>, pub pm: Array1<f64>, // Branch paramaters (Coolant dens., boron conc., fuel and moderator temp.)
    pub xsec: Array4<XBranch>, // Unrodded XSEC
    pub rxsec: Array4<XBranch>, // Rodded XSEC
    pub velo: Array1<f64>, // Neutron velocity
    pub ibeta: Array1<f64>, pub lamb: Array1<f64>, // beta and decay constant
    pub tadf: i32, // Control input: adf
    pub trod: i32, // Control input: control rod
}

pub type MBranchArray = Array1<MBranch>;

#[derive(Debug)]
pub struct Cmfd {
    pub a1n: Array1<f64>, pub a2n: Array1<f64>, pub a3n: Array1<f64>, pub a4n: Array1<f64>, // Nodal expansion coefficients for current node
    pub a1p: Array1<f64>, pub a2p: Array1<f64>, pub a3p: Array1<f64>, pub a4p: Array1<f64>, // Nodal expansion coefficients for following node
    pub ln1: Array1<f64>, pub lp1: Array1<f64>, // First Transverse leakages moments
    pub ndmax: f64, // Maximum change in nodal coupling coefficients
    pub kern: String,
    pub im: i32, pub jm: i32, pub km: i32,
    pub r: Array1<f64>, pub rs: Array1<f64>, pub v: Array1<f64>, pub p: Array1<f64>, pub s: Array1<f64>, pub t: Array1<f64>, pub tmp: Array1<f64>,
}
impl Default for Cmfd {
    fn default() -> Cmfd {
        Cmfd {
            a1n: Array1::zeros(2), a2n: Array1::zeros(2), a3n: Array1::zeros(2), a4n: Array1::zeros(2),
            a1p: Array1::zeros(2), a2p: Array1::zeros(2), a3p: Array1::zeros(2), a4p: Array1::zeros(2),
            ln1: Array1::zeros(2), lp1: Array1::zeros(2),
            ndmax: 0.0,
            kern: String::from("SAMN"),
            im: 0, jm: 0, km: 0,
            r: Array1::zeros(2), rs: Array1::zeros(2), v: Array1::zeros(2), p: Array1::zeros(2), s: Array1::zeros(2), t: Array1::zeros(2), tmp: Array1::zeros(2),
        }
    }
    
}
#[derive(Debug, Default)]
pub struct Timing {
    pub fdm_time: f64,
    pub nod_time: f64,
    pub xs_time: f64,
    pub inp_time: f64,
    pub th_time: f64,
}
impl Timing {
    pub fn get_time() -> f64 {
        let now = Instant::now();
        // Return the elapsed time in seconds as f64
        now.elapsed().as_secs_f64()
    }
}

#[derive(Debug, Default)]
pub struct SData {
    pub mode: String,
    pub ng: i32,
    pub nmat: i32,
    pub x_sec_node: XSecNode,
    pub x_sec_material: XSecMaterial,
    pub geometry: Geometry,
    pub fdm_matrix: FdmMatr,
    pub keff_flux_currents: KeffFluxCurrents,
    pub extra_sources: ExtraSources,
    pub iteration_control: IterationControl,
    pub output_print_option: OutputPrintOption,
    pub fuel_temperature: FuelTemperature, 
    pub moderator_temperature: ModeratorTemperature,
    pub coolent_density: CoolentDensity,
    pub crod_changes: CrodChanges,
    pub boron_concentration: BoronConcentration,
    pub transient_parameters: TransientParameters,
    pub thermal_hydraulics_parameters: ThermalHydraulicsParameters,
    pub steam_table_data: SteamTableData,
    pub m: MBranchArray,
    pub cmfd: Cmfd,
    pub timing: Timing,

}