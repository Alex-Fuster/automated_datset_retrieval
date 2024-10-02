from func_dataset_retrieval import retrieve_zenodo, excel_data_to_dict
import time

# reading queries
path_query='./data/df_query.xlsx'
queries = excel_data_to_dict(path_query)

# retieval process
df_zenodo = retrieve_zenodo(queries)

# saving result in a file
sufix =  time.time()
name = f'zenodo_{sufix}.csv'
df_zenodo.to_csv(name, sep=",", header=True, index=False)





