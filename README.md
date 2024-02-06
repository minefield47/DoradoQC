# DoradoQC: Statistical Analysis of Dorado Summary Files

## Dependencies

```{Required Libraries:}
ggplot2
readxl
tools
ggExtra
dplyr


Install Script for R: 
install.packages(c("ggplot2","readxl", "tools", "ggExtra", "dplyr"))

```


## Execution: 

```{Execution}
Rscript <path/to/DoradoQC_simplex.R> <path/to/summary_file(s)>
Example:
 Rscript ~/DoradoStats/DoradoQC_simplex.R ~/DoradoStats/bp_g1_summary
```

## Output
DoradoQC_Simplex will currently output four graphs:
### Mean Quality Score Histogram
![Mean Quality Score Histogram](bp_g1_summary_DoradoQC_simplex/meanqscore_histogram.png)
### Run Time vs Cumulative Reads
![Run Time vs Cumulative Reads](bp_g1_summary_DoradoQC_simplex/runtime_vs_cumulativereads_filtered.png)
### Run Time vs Cumulative Reads
![Sequence Length Histogram](bp_g1_summary_DoradoQC_simplex/sequencelength_histogram.png)
### Run Time vs Cumulative Reads
![Scatterplot of Read Length vs Mean Q-Score with Histogram Margins](bp_g1_summary_DoradoQC_simplex/sequencelength_qscore_scatter_with_histogram.png)
