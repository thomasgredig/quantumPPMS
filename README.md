# quantumPPMS

Analyzes data files (.dat) generated from VSM modules in the Quantum Design (QD) Physical Property Measurement System (PPMS).

The main functions for **VSM** include:


## Installation

```R
# install.packages("devtools")
devtools::install_github("thomasgredig/quantumPPMS")
```

Type `?? ppms.load` to learn more about all functions that are available.


## Example:

Loading data from a Quantum Design PPMS data file.

```R
library(quantumPPMS)
fname = file.path('.',dir('.','.dat$')[1])
d = ppms.load(fname)
```

## Hysteresis Loops

Finding data files is easy with the `raw.findFiles` function from the [checkRAWfolder package](https://github.com/thomasgredig/checkRAWfolder).

```R
library(quantumPPMS)
file.list = raw.findFiles(path.RAW, instrument='vsm')
fname = file.path(path.RAW, file.list[1])
d = ppms.load(fname)
d = get.vsm.sweepData(d)  # adds a column sweepData
d1 = subset(d, sweepData == 1)
d1$Temp = factor(signif(d1$T,2))
ggplot(d1, aes(H,M,col=Temp)) + geom_point()
```

## Sample Name

Sample names usually start with the initials of the author, followed by the 8-digit date in the format of *yyyymmdd*, see [Naming Guidelines](https://github.com/thomasgredig/MSthesis-Guidelines). It can also be retrieved with the `ppms.dat.info2` function:

```R
library(quantumPPMS)
file.list = raw.findFiles(path.RAW, instrument='vsm')
fname = file.path(path.RAW, file.list[1])
d = ppms.dat.info2(fname)
d$sample.name
```

or the sample name is the 5th part of the filename:

```R
sapply(strsplit(file.list,'_'),'[[',5)
```

