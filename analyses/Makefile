all: results

# Run analyses and generate statistics
stats/e1/: experiment_1/run_comparisons.r
	Rscript $<

stats/e1_bias_irt/: experiment_1/bias_irt_model.r
	Rscript $<

stats/e1_exploratory/: experiment_1/exploratory_analyses.r
	Rscript $<

stats/power_analysis/: power_analysis/run_power_analysis.r
	Rscript $<

stats/e2_perceptual_priming/: experiment_2/perceptual_priming.r
	Rscript $<

stats/e2/: experiment_2/run_comparisons.r
	Rscript $<

stats/e2_exploratory/: experiment_2/exploratory_analyses.r
	Rscript $<

# Generate the plots
plots/e1/: experiment_1/make_plots.r
	Rscript $<

plots/e2/: experiment_2/make_plots.r
	Rscript $<

plots/power_analysis/: power_analysis/make_plots.r
	Rscript $<

results: \
	stats/e1/ \
	stats/e1_bias_irt/ \
	stats/e1_exploratory/ \
	stats/power_analysis/ \
	stats/e2_perceptual_priming/ \
	stats/e2/ \
	stats/e2_exploratory/ \
	plots/e1/ \
	plots/e2/ \
	plots/power_analysis/
