# Jamovi Modules

[Jamovi](https://www.jamovi.org/) is a free and open source project, and developers can create new modules in Jamovi.

#### References:
- https://dev.jamovi.org/tuts0101-getting-started.html

This DoE module contains 4 different design categories such as Factorial Design, Responsive Surface Design, Mixture Design and Taguchi Design.

### Table of contents
1. Design of Experiments (DoE)
    - Factorial Designs
      - Full Factorial
      - Fractional Factorial
    - Responsive Surface Designs
      - Box Behnken
      - Central Composite
    - Mixture Designs
      - Simplex Centroid
      - Extreme Vertices
      - Simplex Lattice
    - More Designs
      - Taguchi
    - Models
      - Linear Model (Linear Regression, ANOVA, Pareto Plot, Contour Plot)
      - Mixture Model (Mixture Model, Mixture Contour Plot)
    - Additional Features for Response
      - Response Means
      - Response Optimization
    - More Plots
      - Main Effects and Interaction Plots
      - Normal QQ Plot


## How to create this DoE module

1. Open R or RStudio

2. Install **jmvtools**

```
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
```

3. Set options

```
# Windows
options(jamovi_home='C:\\Program Files\\jamovi 2.2.5.0')

# MacOS
options(jamovi_home='/Applications/jamovi.app/Contents')
```

4. Set a current directory

```
setwd('YOUR_DIRECTORY/jamovi-modules/DoE')
```

5. Run the jmvtools

```
jmvtools::install()
```
