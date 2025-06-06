# superb 1.0.0 (anytime soon)

* Codename: 100% confident;
* Celebrating the fourth anniversary of `superb`! 
* Corrected minor typos in documentations;

# superb 0.95.99 (May 2025)

* Added an option to `superbShiny()` to preload graphic directives

# superb 0.95.23 (January 17th, 2025)

* Codename: living dangerously;
* Corrected a minor bug when performing Bartlett test
* EXPERIMENTAL: Made allowance to accept namespace::function to the 
  statistics functions. Useful if the function may be masked 
  by other packages declaring functions with the same name;
* Updated options so that EXPERIMENTAL messages can be inhibited;
* Added `range(low, high)` to the `GRD()`'s `Instrument` specification.
* Made "line" the default `plotLayout` instead of "bar"
* Deprecated `plotStyle` in favor of `plotLayout`;

# superb 0.95.22 (December 30th, 2024)

* Minor bug to `superbToWide()`: if a missing subject was the
  first of a group, that was converting to long poorly with `reshape()`.
* Modified `GRD()` to generate within-subject factors with the first 
  factor cycling more rapidly. This is consistent with `dcast()` and
  other long-to-wide functions;

# superb 0.95.21 (December 4th, 2024)

* Made `superb()` determine automatically the `WSDesign` in case the 
    design is not full factorial and the data are long;
* Corrected a bug for mixed designs with long format;
* Upgraded option `colorize` in "lineindividualplot" layout;
* Added a check in `GRD()` to validate that effects bears on legitimate factors.

# superb 0.95.20 (November 26th, 2024)

* Added one validation to `GRD()` to exclude DV same as IV.
* Added `Instrument` to `GRD()` to handle instrument-specific impacts
  on the generated scores (for example, the instrument may have
  only a limited precision)
  
# superb 0.95.19 (October 31st, 2024)

* Codename: Spooky
* Remove deprecated arguments in ggplot2 >=3.5.0
* Added circular layouts for radar plots with error bars
* Cleaned a bit the documentation
* Upgraded a bit the ReadMe page
* Caught a bug in `GRD()` when more than 9 levels where used on an effect
* Cleaned a bit the `geom_superberrorbar()` and added the possibility to 
  have two distinct colors in the vertical part of the error bar.

# superb 0.95.18 (October 15th, 2024)

* Corrected a bug with ``geom_superberrorbar()``

# superb 0.95.17 (September 31th, 2024)

* Added ``superb()``, a formula-based function.
* Modified separator from '_' to '.' in `superbToWide()`

# superb 0.95.16 (September 13th, 2024)

* Added a new plot layout: the corset plot "corset";
* Corrected a bug in the ``superbShiny()`` graphical interface with "corset".

# superb 0.95.15 (August 18th, 2024)

* Added the "antagonize" (boolean) option to violin plots.

# superb 0.95.14 (August 11th, 2024)

* Updated the use of & vs. && according to new R credo
* Corrected a bug related to LD adjustment in mixed designs

# superb 0.95.13 (August 3rd, 2024)

* Modified violin plots so that half-violins can be displayed
* The `violinParams` now includes 
  - direction (+1= left-half; -1=right-half; 0=symmetrical);
  - push (to translate half away from the medial line)
  - with these modifications, corset plots can be made easily
    by superimposing a violin plot with halves and individualline plot

# superb 0.95.12 (May 30th, 2024)

* Revise FYI message for un-balanced between-group designs
* Corrected a small bug for un-balanced within-subject designs
* Added a check for correlations on data without variance

# superb 0.95.11 (May 7th, 2024)

* Added local decorrelation with option "LDr" where r is the radius.
* Small correction to `lineBand` without between-subject factors

# superb 0.95.10 (March 9th, 2024)

* Regular maintenance.

# superb 0.95.9 (February 8th, 2024)

* Added a new plot layout, `boxplot`, to display box plots.
* Adjusted testthat tests following new release of ggplot2
* Added unitaryAlpha technique to compute average correlation
* Simplified documentation for within-subject designs.

# superb 0.95.83 (November 11th, 2023)

* Made a message shown based on option

# superb 0.95.82 (August 19th, 2023)

* Corrected labeling of factors.

# superb 0.95.81 (August 13th, 2023)

* Updated CustomizingSuperbPlots vignettes.

# superb 0.95.8 (June 13th, 2023)

* Added `superbToWide()` to ease the conversion to wide format
* A few tiny corrections to the documentation
* Updated references
* Note that updates are spaced more and more as the library is now very stable

# superb 0.95.7 (January 19th, 2023)

* Added pairwise deletion for the computation of the mean correlation in case of missing data with CA
* Removed ggplot2 depleted functions "aes_string" in favor of "aes"
* Removed ggplot2 depleted attribute "size" in favor of "linewidth"
* Added the `lineBand` plot in the Shiny interface

# superb 0.95.6 (December 24th, 2022)

* Added documentation related to plotting frequencies
* Harmonized a bit the other vignettes' format.
* Updated version in shinyapps.io

# superb 0.95.5 (November 18th, 2022)

* Tweaked column selection so that the variable argument can be
  given columns names in different order than the order in the dataframe. 

# superb 0.95.4 (September 9th, 2022)

* tweaked "pointing" so that for error bars not lying on both side of the central tendency, 
  the line does not pass throught the central tendency.

# superb 0.95.3 (July 9th, 2022)

* Updated documentation.

# superb 0.95.2 (July 9th, 2022)

* Added an option "pointing" to error bars so that they can be pointing;
  "up", "down" or go in "both" directions (default).

# superb 0.95.1 (May 12th, 2022)

* Corrected the bug from 0.9.7.9 (kept bugging me!);
* Reduced image resolution in vignettes.

# superb 0.95.0 (May 10th, 2022)

* Added a new layout to display precision using a band rather than error bars;
* changed numbering of versions to two-digits version code.

# superb 0.9.7.9 (January 15th, 2022)

* Corrected a problem in the order of the conditions when more then 10 were present.

# superb 0.9.7.8 (December 10th, 2021)

* Corrected a few typos;
* The population size in `superbShiny()` was limited to 99,999. Corrected.

# superb 0.9.7.7 (November 19th, 2021)

* Added a vignette for superb with SPSS;
* Corrected `superbShiny()` to show the doi and remove graphic directives;
* Updated the shinyapps.io version to match this version.

# superb 0.9.7.6 (November 4th, 2021)

* Corrected one bug regarding variables with similar names;
* Integrated tibble data format into superb;
* Some output were missing in VignetteC and a backtick missing in VignetteA.

# superb 0.9.7.5 (June 23rd, 2021)

* Codename: "two-tail 95% confident"
* Official release on CRAN of ``superb`` with the graphical user interface
``superbShiny()``;
* Small changes to vignette TheMakingOf.

# superb 0.9.7.4 (June 18th, 2021)

* Beta release of ``superbShiny()``, a graphical user interface to facilitate the
  use of superbPlot.

# superb 0.9.7.0 (May 28th, 2021)

* Added a vignette on how to implement Reference Intervals (RI) into ``superb``;
* Implemented ``makeTransparent()`` to ease the creation of plots with multiple error bars;
* Added a distinct vertical color, ``vcolor``, to the ``geom_superberrorbar()``;
* Added an argument "WSDesign" (default = "fullfactorial") when the within-subject
    factors are not full-factorial;
* Added a vignette on how to plot non-full factorial datasets.

# superb 0.9.6.0 (April 30th, 2021)

* Integrated an example of robust statistic in Vignette/Article 4: 
   the 20% trimmed mean and its confidence interval;
* Added ``showSignificance()`` to annotate group differences;
* Added a note when missing data are in the dataframe;
* Added automatic handling of initializers (see Vignette/Article 9 for an example);
* Added a vignette dedicated to how to illustrate Cohen's d;
* Added a ``geom_superberrorbar()`` with additional options for the tip markers;
* ... all this on its way to the next release version 0.9.7.5. 
* Stay tune as the best is still to come (and it is shiny!)

# superb 0.9.5.0 (April 13th, 2021)

* Second release on CRAN, codename "95% confident";
* Expanded documentation once more.

# superb 0.9.4.7 (April 10th, 2021)

* Added two vignettes.

# superb 0.9.4.6 (April 7th, 2021)

* Problem with CAPITAL grd corrected;
* Rounded the numbers in the messages to 4 decimals (that should be more than enough!);
* All the message/warnings/stop generated by suberb are now prefixed with "superb::";
* Added function `WelchDegreeOfFreedom()`;
* Added a message that indicate tryon adjustment value;
* Added a vignette on Welch, Tryon & superb.

# superb 0.9.4.5 (April 1st, 2021)

* Implemented the raincloud plots;
* Converted messages from warning() to message().

# superb 0.9.4.4 (March 2021)

* Expanded function names lambda, epsilon;
* Made built-in bootstrap estimators for SE and PI;
* Renamed option "debug" into "feedback";
* Changed debug information for `GRD()`;
* Converted in GRD the level ranges, e.g., "diff(1,5)", into numeric levels.

# superb 0.9.4.3 (March 2021)

* Added `superbData()` as a shortcut to `superbPlot(... showPlot = FALSE)`;
* Changed debug information for `superbPlot()` and `superbData()`;
* Created a logo and improved web pages;
* Revised documentation.

# superb 0.9.4.2 (March 2021)

* Initial release of the R version on CRAN;
* Added optional argument `facetParams` to the plotting functions;
* Made the plot function customizable;
* Wrote vignettes.

# superb 0.9.4.0 (September 2020)

* Beta release of the package `superb` on GitHub;
* `MeanPlot` renamed to `superb` (SUmmary Plot with ERror Bars);
* Added pre and post-processing operators;
* Packaged together `GRD` and `MeanPlot`;
* Transcoded `MeanPlot` from Mathematica to R.

# superb 0.0.5.0  (January 2019)

* Initial release of `GRD()` 2.0 for the R platform.

# superb 0.0.4.0  (March 2017)

* Last stable version maintained on Mathematica; Cousineau (2017) doi: 10.5709/acp-0214-z;
* Updated legend manipulation following the release of Mathematica 9.0.

# superb 0.0.3.0  (October 2015)

* Release of GRD 2.0 for SPSS.

# superb 0.0.2.0  (July 2014)

* Release of GRD 1.0 for SPSS.

# superb 0.0.1.0  (and prior; 2009-2015)

* Development versions of `MeanPlot` on the Mathematica framework;
* Added "popSize" and "CRS" from Cousineau & Laurencelle (2016) doi: 10.1037/met0000055;
* Grouped the various adjustments into an "adjustments" list;
* Added "LM" decorrelation method.
