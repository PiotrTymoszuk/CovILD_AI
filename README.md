# CovILD_AI
Assessment of radiological abnormalities and prediction of lung function with AI-determined lung opacity

## Summary

<p align = "center"> 
<img src = "https://github.com/PiotrTymoszuk/CovILD_AI/assets/80723424/ca6b16bc-fa19-4676-9163-cf889ef829ab" width = "65%">
</p>

### Analysis report

You can access figures of the analysis reort in high resolution PDFs and raw Excel tables [here](https://github.com/PiotrTymoszuk/CovILD_AI/tree/main/report). 
The anaylsis report is available from a [Dropbox](https://www.dropbox.com/scl/fo/k3pb2r24lvskfwg9yqfa5/h?rlkey=fixs1601asyzw6g5p7d5akewz&dl=0): note, please, download the analysis report in HTML format and open it in your web browser.

### Manuscript draft

Draft of the manuscript in Word format is available [here](https://github.com/PiotrTymoszuk/CovILD_AI/blob/main/paper/manuscript.docx). 
Supplementary material is provided as a [Word file](https://github.com/PiotrTymoszuk/CovILD_AI/blob/main/paper/supplementary_material.docx). 
Backbone of the rebuttal letter is available as a [Word file](https://github.com/PiotrTymoszuk/CovILD_AI/blob/main/paper/rebuttal_letter.docx).
High resolution figures, supplementary figures and an Excel file with supplementary tables are provided in [paper folder](https://github.com/PiotrTymoszuk/CovILD_AI/tree/main/paper).

## Terms of use

Please cite the repository and the peer-reviewed publication, when available. The raw data files will be made upon request to the study authors, [Dr. Anna Luger](mailto:Anna.Luger@i-med.ac.at), [Dr. Christoph Schwabl](mailto:christoph.schwabl@i-med.ac.at), [Dr. Gerlig Widmann](mailto:gerlig.widmann@i-med.ac.at).

## Usage

The following development packages are required to run the pipeline:

```r

devtools::install_github('PiotrTymoszuk/soucer') ## script sourcing
devtools::install_github('PiotrTymoszuk/ExDA') ## exploratory data analysis and staristical hypothesis testing
devtools::install_github('PiotrTymoszuk/figur') ## management of figures and tables in Rmd documents
devtools::install_github('PiotrTymoszuk/trafo') ## handling of tabular data
devtools::install_github('PiotrTymoszuk/caretExtra') ## performance stats and visualization of caret models
devtools::install_github('PiotrTymoszuk/clustTools') ## used in correspondence analysis
devtools::install_github('PiotrTymoszuk/bootStat') ## boostrap/blocked bootstrap for ROC, inter-rater reliability stats and hypothesis testing
devtools::install_github('PiotrTymoszuk/graphExtra') ## correlation graphs

devtools::install_github('PiotrTymoszuk/somKernels') ## a dependency of `caretExtra`

```

Source 'exec.R' to launch the entire pipeline:

```r

source('exec.R')

```

## Contact

The repository maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Dr. Christoph Schwabl](mailto:christoph.schwabl@i-med.ac.at) and [Dr. Gerlig Widmann](mailto:gerlig.widmann@i-med.ac.at).

<br>
