# CovILD_AI
Assessment of radiological abnormalities and prediction of lung function with AI-determined lung opacity

<br>

## Summary

You can follow the analysis progress [here](https://github.com/PiotrTymoszuk/CovILD_AI/tree/main/report). 
Note: please download [the analysis report in HTML format](https://github.com/PiotrTymoszuk/CovILD_AI/blob/main/report/report.html) and open in in your web browser.

<br>

## Terms of use

Please cite the repository and the peer-reviewed publication, when available. The raw data files will be made upon request to the senior study authors, [Dr. Christoph Schwabl](mailto:christoph.schwabl@i-med.ac.at) and [Dr. Gerlig Widmann](mailto:gerlig.widmann@i-med.ac.at).

<br>

## Basic usage

The following development packages are required to run the pipeline:

```r

devtools::install_github('PiotrTymoszuk/soucer') ## script sourcing
devtools::install_github('PiotrTymoszuk/ExDA') ## exploratory data analysis and staristical hypothesis testing
devtools::install_github('PiotrTymoszuk/figur') ## management of figures and tables in Rmd documents
devtools::install_github('PiotrTymoszuk/trafo') ## handling of tabular data
devtools::install_github('PiotrTymoszuk/caretExtra') ## performance stats and visualization of caret models

```

Source 'exec.R' to launch the entire pipeline:

```r

source('exec.R')

```
<br>

## Contact

The repository maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Dr. Christoph Schwabl](mailto:christoph.schwabl@i-med.ac.at) and [Dr. Gerlig Widmann](mailto:gerlig.widmann@i-med.ac.at).

<br>
