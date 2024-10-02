from func_dataset_retrieval import retrieve_semantic, create_file, excel_data_to_dict
import time


# reading queries
path_query='./data/df_query.xlsx'
queries = excel_data_to_dict(path_query)
offset = 0
limit = 50
year_min = 1980
year_max = 2022

# retrieval process
df_semantic, errors = retrieve_semantic(queries, offset = offset, limit = limit, year_min = year_min, year_max = year_max)

# saving file
sufix =  time.time()
name = f'semantic_scholar_{sufix}.csv'
df_semantic.to_csv(name, sep=",", header=True, index=False)

errorsFilename = f'errors_{sufix}.json'
create_file(errorsFilename, errors)





