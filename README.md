# Bias amplification in social networks

Pre-registrations, experiment demos, data, analyses, and simulations for <b>Experimental evidence for bias amplification in social networks and a mitigation framework</b> by [Matt Hardy](https://matthardy.org/), [Bill Thompson](https://billdthompson.github.io/), [Peaks Krafft](https://www.arts.ac.uk/creative-computing-institute/people/peaks-krafft), and [Tom Griffiths](https://cocosci.princeton.edu).

## Pre-registration links

* Experiment 1: https://osf.io/yth5r
* Experiment 2: https://osf.io/87me6/

## Experiment demo

Perform the experiment as they were given to participants at the link below. Note that the participant interface for the social-resampling condition in Experiment 2 was identical to the social-motivated condition.

[Experiment demo](https://bias-amplification.netlify.app)

## Experiment code

Code for both experiments is available in the experiments/ directory. Experiments were run using Dallinger. To test an experiment locally, navigate to the experiment directory, and then run: 

```
$ pip install -r requirements.txt
$ dallinger debug --verbose
```

Note that installing Dallinger can be buggy and there are often version conficts between required packages. More documentation is given on their [website](https://dallinger.readthedocs.io/en/latest).

## Data

Data from both experiments can be found in the data/ directory:

* Experiment 1: [Experiment 1.csv](data/experiment_1.csv)
* Experiment 2: [experiment_2.csv](data/experiment_2.csv)

Data for each of the 100 simulations of Experiment 2 used for the power analysis can be found in the data/simulated_data/ directory.

## Analyses

In the analyses/ directory, running `$ make results` will generate all the statistics and plots used in the paper. Statistics reported in the paper are computed are stored in analyses/stats/ as TeX files, and plots are stored in analyses/plots/.

### Statistical analyses
* Experiment 1 main analyses: [e1_main_analyses.r](analyses/experiment_1/e1_main_analyses.R)
* Experiment 1 exploratory analyses: [e1_exploratory_analyses.r](analyses/experiment_1/e1_exploratory_analyses.R)
* Experiment 1 IRT model: [bias_irt_model.r](analyses/experiment_1/bias_irt_model.R)
* Experiments 1 and 2 perceptual priming exploratory analyses: [perceptual_priming.r](analyses/experiment_2/perceptual_priming.R)
* Experiment 2 power analysis: [run_power_analysis.r](analyses/power_analysis/run_power_analysis.R)
* Experiment 2 main analyses: [e2_main_analyses.r](analyses/experiment_2/e2_main_analyses.R)
* Experiment 2 exploratory analyses: [e2_exploratory_analyses.r](analyses/experiment_2/e2_exploratory_analyses.R)

### Plots
* Experiment 1 plots: [e1_plots.r](analyses/experiment_1/e1_plots.R)
* Experiment 2 plots: [e2_plots.r](analyses/experiment_2/e2_plots.R)
* Power analysis plots: [power_analysis_plots.r](analyses/power_analysis/power_analysis_plots.R)

Note that the Makefile will not generate the Bayesian model figure. To generate this figure, run the [make_model_plot.m](analyses/bayesian_model/make_model_plot.m) MATLAB script.

## Experiment simulation

To simulate the experiment (used for the Experiment 2 power analysis), navigate to the simulation/ directory and run `$ Rscript simulate_experiment.R <simulation_id> <output_directory>`. `simulation_id` is an integer that serves as a unique identifier for the simulation. `output_directory` is a string representing the directory where the data from the simulation should be stored, e.g. `$ Rscript simulate_experiment.R 0 test_directory`. Data from the 100 runs of this script we used for the power analysis is in the data/analyses/ directory. Note that by default, the simulation uses the cached oracle models in the simulation/fitted_oracles/ directory to simulate participant behavior.