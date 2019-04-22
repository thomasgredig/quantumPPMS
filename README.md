# quantumPPMS

Analyzes data files (.dat) generated from VSM modules in the Quantum Design (QD) Physical Property Measurement System (PPMS).


## Installation

```R
# install.packages("devtools")
devtools::install_github("thomasgredig/quantumPPMS")
```


## Example:

```R
library(quantumPPMS)
fname = file.path('.',dir('.','.dat$'))
d = ppms.load(fname)
```
