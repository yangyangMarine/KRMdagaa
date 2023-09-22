
# KRM modelling of dagaa

<img src="https://github.com/yangyangMarine/KRMdagaa/blob/162ebcbc34ee2afc6a0074301b9885451def7cc4/Figures/StA.jpg" align="center" width="300" />
Yang Yang, [University of St Andrews](https://www.st-andrews.ac.uk/)

## Description
Materials underpinning the publication: Yang Yang, Sven Gastauer, Roland Proud, Richard Mangeni-Sande, Inigo Everson, Robert J Kayanda, Andrew S Brierley, Modelling and in situ observation of broadband acoustic scattering from the Silver cyprinid (_Rastrineobola argentea_) in Lake Victoria, East Africa, ICES Journal of Marine Science, 2023. (https://doi.org/10.1093/icesjms/fsad137)
* Lake Victoria
<img src="https://github.com/yangyangMarine/KRMdagaa/blob/162ebcbc34ee2afc6a0074301b9885451def7cc4/Figures/LV.jpg" width="400" /> 

* Silver cyprinid (_Rastrineobola argentea_)
<img src="https://github.com/yangyangMarine/KRMdagaa/blob/162ebcbc34ee2afc6a0074301b9885451def7cc4/Figures/dagaa.jpg" width="400" />

## Getting Started

### Dependencies

* Loading dependencies
```
pacman::p_load(KRMr,ggplot2, readxl,truncnorm,dplyr,reshape2,colorRamps,plotly,abind)
```

### Executing program

```
  source(paste0(dir, "shpplot_yang.R"))
  source(paste0(dir, "krm.dagaa.R"))
  source(paste0(dir, "krm.dagaa.sim.R"))
```

## Help


## License

This project is licensed under the MIT License - see the LICENSE.md file for details

## References:

* Clay, C. S., & Horne, J. K. (1992). Acoustic models and target strengths of the Atlantic cod (Gadus morhua). The Journal of the Acoustical Society of America, 92, 2350-2351.
