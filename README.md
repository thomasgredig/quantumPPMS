# quantumPPMS

Analyzes data files (.dat) generated from VSM modules in the Physical Property Measurement System (PPMS). Extensive documentation is available in the [reference manual](https://thomasgredig.github.io/quantumPPMS/).


## Installation

```R
# install.packages("devtools")
devtools::install_github("thomasgredig/quantumPPMS")
```

## Usage

Loading data from a Quantum Design PPMS data file.

```R
library(quantumPPMS)
filename = ppms.getSampleFiles()[1]
d = vsm.import(filename)
summary(d)
plot(d)
```

## Predecated functions

Some functions in version 0.2 and earlier had different names; use the new functions instead, if help is needed on how to convert the old function, then use `vsm.help()`.

| old function               | new function |
|----------------------------|--------------|
| analyze.single.VSM.loop    |   vsm.analyzeLoop  |
| ppms.dat.info2  | vsm.info |
| vsm.hyst.stats  | vsm.hystStats |
| ppms.load | vsm.import |

   

## Hysteresis Loops

Finding data files is easy with the `raw.findFiles` function from the [checkRAWfolder package](https://github.com/thomasgredig/checkRAWfolder).

```R
library(quantumPPMS)
fname = vsm.getSampleFiles()[1]
d = vsm.import(fname)

print(d)
plot(d)
```


