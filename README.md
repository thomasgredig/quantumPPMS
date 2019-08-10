# quantumPPMS

Analyzes data files (.dat) generated from VSM modules in the Quantum Design (QD) Physical Property Measurement System (PPMS).

The main functions for **VSM** include:


## Installation

```R
# install.packages("devtools")
devtools::install_github("thomasgredig/quantumPPMS")
```


## Example:

```R
library(quantumPPMS)
fname = file.path('.',dir('.','.dat$')[1])
d = ppms.load(fname)
```

## Hysteresis Loops

```R
library(quantumPPMS)
fname = file.path('.',dir('.','.dat$')[1])
d = ppms.load(fname)
d = get.vsm.sweepData(d)
d1 = subset(d, sweepData == 1)
d1$Temp = factor(signif(d1$T,2))
ggplot(d1, aes(H,M,col=Temp) + geom_point()
```

## Sample Name

Sample names usually start with the initials of the author, followed by the 8-digit date in the format of *yyyymmdd*. The sample name can be guessed in the following way (unless it is in the filename):

```R
library(quantumPPMS)
fname = file.path('.',dir('.','.dat$')[1])
d = ppms.dat.info2(fname)
d$sample.name
```

[//]: <> (Run pkgdown::build_site())
