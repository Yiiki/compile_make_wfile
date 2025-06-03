# wfile maker

Used to generate wfile from atom X pseudopotential files.

This wfiles is used as the decay function when extracting PMs from bulk microscopic polarization.

## table

* PBE psp

| element | atom | psp file        | rho0 | a0  |
| ------- | ---- | --------------- | ---- | --  |
| Si      | 14   | Si.SG15.PBE.UPF | 0.03 | 1.2 |
| H       | 1    |  H.SG15.PBE.UPF | 0.00 | 1.0 |
| Ge      | 32   | Ge.SG15.PBE.UPF | 0.60 | 1.2 |

* VWR psp

| element | atom | psp file        | rho0 | a0  |
| ------- | ---- | --------------- | ---- | --  |
| Si      | 14   | vwr.Si.UPF      | 0.03 | 1.2 |
| H       | 1    | vwr.H.UPF       | 0.00 | 1.0 |
| O       | 8    | vwr.O.UPF       | 0.45 | 0.3 |
| Au      | 79   | vwr.Au.UPF      | 0.40 | 0.6 |
