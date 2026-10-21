# Dataset

Obtain **StudentPerformanceFactors.csv** from the [Student Performance Factors dataset by Lai Nguyen on Kaggle](https://www.kaggle.com/datasets/lainguyn123/student-performance-factors). Place the unmodified file in this directory before running `analysis.R`.

The original CSV is not redistributed in this repository. Refer to the publisher for its current terms and data dictionary. The Shiny demonstration works without downloading the CSV because the fitted model is included.

The supplied file has 6,607 rows and 20 columns. Under the stated 0–100 score-scale assumption, one score of 101 is quarantined, leaving 6,606 analysis rows. No actual student identifier or verified measurement dates are supplied. Generated record numbers identify file rows only.

A [published synthetic-data study](https://doi.org/10.3390/info16090763) identifies the matching named dataset and describes its synthetic provenance. The results therefore demonstrate a statistical workflow and cannot establish performance at a real education provider.
