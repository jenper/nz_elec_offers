# nz_elec_offers

This package aims to create a report to display metrics of NZ electricity generators offers and identify any periods of unusual offers and when the underlying trend of offers seems to change.  This is to identify when electricity generators are making offers that deviate from their usual offers which could potentially signal events that have upset the electricity market (if all generators show changes at the same time) or that generators are making uncompetitive offers (if for example offers suddenly increase while market conditions remain the same). 

While the default data is on NZ electricity generator offers the functions can be applied to any situation where someone may want to detect for anomalies or change points in a time series and compare those against other times series. 

Note the difference between the change_points and anomaly_detect functions are that the former suggests a sustained change while the latter suggests a short-term anomaly. change_points identifies time steps when one model changes to a new model and anomaly_detect identifies time steps that deviate significantly from a single model. 

To install its recommended to use the remotes::install_github("jenper/offers", INSTALL_opts="--install-tests", type='source', force=TRUE, build_vignettes = TRUE, dependencies=TRUE) command. 

