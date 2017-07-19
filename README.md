# Modelling the burden of hepatitis C infection among people who inject drugs in Norway, 1973-2030

## Article
### Authors

Hinta Meijerink
- Norwegian Institute of Public Health, Oslo, Norway
- European Programme for Intervention Epidemiology Training (EPIET), European Centre for Disease Prevention and Control, (ECDC), Stockholm, Sweden

Richard A White
- Norwegian Institute of Public Health, Oslo, Norway

Astrid Løvlie
- Norwegian Institute of Public Health, Oslo, Norway

Birgitte Freiesleben de Blasio
- Norwegian Institute of Public Health, Oslo, Norway
- Oslo Centre for Biostatistics and Epidemiology, Dept. of Biostatistics, Institute of Basic Medical Sciences, University of Oslo, Oslo, Norway

Olav Dalgard
- Akershus University Hospital, and Medical Faculty, Oslo University

Ellen J. Amundsen
- Norwegian Institute of Public Health, Oslo, Norway

Espen Melum
- Norwegian PSC Research Center, Division of Surgery, Inflammatory Medicine and Transplantation, Oslo University Hospital, Rikshospitalet, Oslo, Norway
- Section of Gastroenterology, Division of Surgery, Inflammatory Medicine and Transplantation, Oslo University Hospital, Rikshospitalet, Oslo, Norway
- Research Institute of Internal Medicine, Division of Surgery, Inflammatory Medicine and Transplantation, Oslo University Hospital, Rikshospitalet, Oslo, Norway

Hilde Kløvstad
- Norwegian Institute of Public Health, Oslo, Norway


### Background

Lack of Hepatitis C virus (HCV) incidence data in (Norwegian) high-risk groups impedes the ability to make informed decisions on prevention measures. Thus we rely on modelling to estimate the incidence and burden of HCV infections.

### Methods

We constructed a compartmental model for HCV infections in Norway among active and former people who inject drugs (PWIDs). We based yearly transition rates on literature. The model was fitted to absolute numbers of hepatitis C associated cirrhosis, hepatocellular carcinoma (HCC) and death from national data sources (2000 -2013). We estimated the number (95%CI) of HCV infections, cirrhosis, HCC and death and disability adjusted life years (DALYs) due to HCV infections in Norway, 1973-2030. We assumed treatment rates in the projected period were similar to those in 2013.

## Running the model
### Dependencies

- R v3.4.0
- data.table
- pomp
- tidyr
- htmlTable
- RAWmisc
- devtools
- ggplot2

`
install.packages(c("data.table", "pomp", "tidyr", "htmlTable", "devtools", "ggplot2"))
devtools::install_github("raubreywhite/RAWmisc")
`

### Setup and code execution

1. Clone the repository into your desired folder
2. Open `code/hcv/Run_Linux.R`
3. Edit lines `12-17` to reflect your folder locations
4. Run `code/hcv/Run_Linux.R`

## Results

All results are available at https://github.com/raubreywhite/hcv_burden_paper_2017/tree/master/results_final/hcv




