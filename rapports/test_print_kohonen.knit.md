---
title: "test"
author: "Godefroy Clair"
date: "16 septembre 2016"
output:
  pdf_document:
    includes:
      in_header: ../style/header.tex 
    fig_caption: yes
    latex_engine: xelatex
    number_sections: yes
    toc: yes
    toc_depth: 3
  html_document:
    css: ../style/simple-report.style.css
    number_sections: yes
    toc: yes
  word_document: default
---






#test 1




```
## Loading objects:
##   som1
```

```
## Loading objects:
##   som2
```

```
## Loading objects:
##   som3
```

```
## Loading objects:
##   df_stat_scale_scale
```

```
## Loading objects:
##   df_stat_scale_scale
```

```
## Loading objects:
##   df_stat_scaled
```

```
## Loading objects:
##   df_selec
```


![](test_print_kohonen_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

La carte suivante nous indique le nombre de données captées par chaque neurone.

![](test_print_kohonen_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

Celle-ci nous indique la distance des données à chaque référent. Elle indique donc si les données sont proches entre elles (car un référent mesure pour chaque variable la moyenne des données qu'il a collectées)

![carte de qualité](test_print_kohonen_files/figure-latex/quality_map-1.pdf) 

### Quelques autres graphiques standards

Les graphiques suivants sont des graphiques dit "heatmap" pour chaque variable : ils permettent de voir les zones où cette variable est plus élevée ou au contraire plus basse.

![](test_print_kohonen_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 


![](test_print_kohonen_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

![](test_print_kohonen_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

![](test_print_kohonen_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 
