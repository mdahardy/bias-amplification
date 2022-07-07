# Social bias amplification

Pre-registrations, experiment demos, data, analyses, and simulations for <b>Experimental evidence for bias amplification in social networks and a mitigation framework</b> by [Matt Hardy](https://matthardy.org/), [Bill Thompson](https://billdthompson.github.io/), [Peaks Krafft](https://www.arts.ac.uk/creative-computing-institute/people/peaks-krafft), and [Tom Griffiths](https://cocosci.princeton.edu).

## Pre-registration links

* Experiment 1: https://osf.io/yth5r
* Experiment 2: https://osf.io/87me6/

## Experiment demo

Perform the experiment as they were given to participants at the link below. Note that the participant interface for the social-resampling condition in Experiment 2 was identical to the social-motivated condition.

* [Demo](https://bias-amplification.netlify.app)

## Experiment code

Code for both experiments is available in the experiments/ directory. Experiments were run using Dallinger. To test an experiment locally, navigate to the experiment directory, install the required Python libraries given in `requirements.txt`, and run `dallinger debug --verbose`. More documentation on Dallinger is given on their [website](https://dallinger.readthedocs.io/en/latest).

## Data

Data from both experiments can be found in the data/ directory:

* Experiment 1: [experiment_1.csv](data/experiment_1.csv)
* Experiment 2: [experiment_2.csv](data/experiment_2.csv)

Data for each of the 100 simulations of Experiment 2 used for the power analysis can be found in the data/simulated_data/ directory.

## Analyses

In the analyses/ directory, running `make results` will generate all the statistics and plots used in the paper. First, all of the statistics reported in the paper are computed are stored in analyses/stats/ as TeX files:

* Experiment 1 main analyses: [compare_conditions.R](analyses/experiment_1/compare_conditions.R)
* Experiment 1 exploratory analyses: [exploratory_analyses.R](analyses/experiment_1/exploratory_analyses.R)
* Experiment 1 IRT model: [bias_irt_model.R](analyses/experiment_1/bias_irt_model.R)
* Experiments 1 and 2 perceptual priming exploratory analyses: [perceptual_priming.R](analyses/experiment_2/perceptual_priming.R)
* Experiment 2 power analysis: [power_analysis.R](analyses/experiment_2/compare_conditions.R)
* Experiment 2 main analyses: [compare_conditions.R](analyses/experiment_2/compare_conditions.R)
* Experiment 2 exploratory analyses: [exploratory_analyses.R](analyses/experiment_2/exploratory_analyses.R)

Second, all the plots used in the paper are generated and stored in analyses/plots/: 

* Experiment 1 plots: [make_plots.R](analyses/experiment_1/make_plots.R)
* Experiment 2 plots: [make_plots.R](analyses/experiment_2/make_plots.R)
* Power analysis plots: [make_plots.R](analyses/power_analysis/make_plots.R)

Note that running `make results` will not generate the Bayesian model figure. To generate this figure, you need to run the MATLAB [make_model_plot.m](analyses/bayesian_model/make_model_plot.m) script.

## Experiment simulation

To simulate the experiment (used for the Experiment 2 power analysis), navigate to the simulation/ directory and run `Rscript simulate_experiment.R <simulation_id> <output_directory>`. `simulation_id` is an integer that serves as a unique identifier for the simulation. `output_directory` is a string representing the directory where the data from the simulation should be stored, e.g. `Rscript simulate_experiment.R 0 test_directory`. Note that by default, the simulation uses the cached oracle models in the simulation/fitted_oracles/ directory to simulate participant behavior.