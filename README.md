## Evaluating the feasibility of automating dataset retrieval for biodiversity monitoring

### Alexandre Fuster-Calvo\*, Sarah Valentin , William C Tamayo, Dominique Gravel

\*Contact author: alexfuster7@gmail.com

ABSTRACT

_Aim_

Effective management strategies for conserving biodiversity and mitigating the impacts of Global Change rely on access to comprehensive and up-to-date biodiversity data. However, manual search, retrieval, evaluation, and integration of this information into databases presents a significant challenge to keep pace with the rapid influx of large amounts of data, hindering its utility in contemporary decision-making processes. The automation of these tasks through advanced algorithms holds immense potential to revolutionize biodiversity monitoring. 

_Innovation_ 

In this study, we investigate the potential for automating the retrieval and evaluation of biodiversity data from Dryad and Zenodo repositories. We have designed an evaluation system based on various criteria, including the type of data provided and its spatio-temporal range, and applied it to manually assess the relevance for biodiversity monitoring of datasets retrieved through repository APIs. We evaluated a supervised classification to identify potentially relevant datasets and investigate the feasibility of automatically ranking the relevance. Additionally, we compare our results with those obtained from a scientific literature source, using data from Semantic Scholar for reference. Our evaluation centers on the database utilized by a national biodiversity monitoring system in Quebec, Canada. 

_Main conclusions_ 

The algorithms retrieved 89 (55%) relevant datasets for our database, showing the value of automated dataset search in repositories. Additionally, we find that scientific publication sources offer broader temporal coverage and can serve as conduits guiding researchers toward other valuable data sources. Our automated classification system showed moderate performance in detecting relevant datasets, with an F-score of 0.53, emphasizing the need for further refinement. A key challenge identified in our manual evaluation is the scarcity and uneven distribution of metadata, especially pertaining to spatial and temporal extents. Our evaluative framework, based on predefined criteria, can be adopted by automated algorithms for streamlined prioritization, and we make our manually evaluated data publicly available, serving as a benchmark for improving classification techniques. 



FOLDERS:

- **data** contains the raw data used for the analyses
- **scripts** R script to run analyses
-- **retrieval** Python scripts to run dataset retrieval
-- **automated_classification** Python scripts to run automated process

_Run the code_

- install python
- clone the repository by:
  Via ssh: `git clone git@github.com:Alex-Fuster/automated_datset_retrieval.git`
  Via https: `git clone https://github.com/Alex-Fuster/automated_datset_retrieval.git`

- (optional) create a virtual environment in case you do not want to keep the libraries in your global library storage. `python -m venv /path/to/new/virtual/environment`.[find doc here](https://docs.python.org/3/library/venv.html)

- run the following line to install all necessary libraries: `pip install -r requirements.txt`

For **retrieval** scripts:

- start retrieval case zenodo: `python scripts_retrieval/zenodo.py`

- start retrieval case semantic_scolar: `python scripts_retrieval/semantic_scolar.py`
