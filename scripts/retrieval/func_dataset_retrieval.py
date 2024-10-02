import itertools
import json
import requests
import regex as re
import pandas as pd
import time
import json


def create_file(filename, content):
    with open(filename, 'w') as json_file:
        json.dump(content, json_file, indent=4)

def excel_data_to_dict(execl_path):
    
    df_queries = pd.read_excel(execl_path, engine = "openpyxl", converters={'id_query':int})
    data = {}
    count = 0
    for query in df_queries['query']:
        data[count] = query
        count = count + 1
    
    return data 
    
def conv_query(x):
    l = [y.strip() for y in x.split("AND")]
    if 'Québec' in l:
        l = [l] + [[w.replace('Québec', 'Quebec') for w in l]]
    else:
        l = [l]
    return(l)

def extract_dryad(entry, i_query):
    if "keywords" in entry.keys():
        keywords = "_".join(entry['keywords'])
    else:
        keywords = []
    return([entry['_links']['self']['href'],
    entry["title"],
    entry['abstract'],
    keywords,
    entry["publicationDate"],
           i_query])

def extract_zenodo(entry, i_query):
    
    metadata = entry["metadata"]
    if "keywords" in metadata.keys():
        keywords = "; ".join(metadata['keywords'])
    else:
        keywords = ""
        
    if "method" in metadata.keys():
        method = metadata['method']
    else:
        method = ""
    
    if "notes" in metadata.keys():
        notes = metadata['notes']
    else:
        notes = ""
    
    if "locations" in metadata.keys():
        locations = [k["place"] for k in metadata['locations']]
        locations = "; ".join(locations)
    else:
        locations = ""
    
    if "related_identifiers" in metadata.keys():
        cited_articles = ["https://doi.org/" + k["identifier"] for k in metadata["related_identifiers"]]
        cited_articles = "; ".join(cited_articles)
    else:
        cited_articles = ""
    return([metadata['doi'],
    metadata["title"],
    metadata['description'],
   method,
    notes,        
    keywords,
    locations,
    metadata["publication_date"],
    cited_articles,
           i_query])

def extract_semantic_scholar(entry, i_query):
    #entry = entry.json()
    rows = []
    doi = ""
    if "DOI" in entry["externalIds"].keys():
        doi = entry["externalIds"]["DOI"]
    
    return([doi,
            entry["url"],
    entry["venue"],
    entry["title"],
    entry['abstract'],
    entry["year"],
           i_query])

def extract_semantic_scholar_old(entry, i_query):
    rows = []
    entry = entry.json()["results"]

    for i in range(len(entry)):
        if "pubDate" in entry[i].keys():
            date = entry[i]["pubDate"]
        else:
            date = entry[i]["year"]["text"]

        if "doiInfo" in entry[i].keys():
            url = entry[i]["doiInfo"]['doiUrl']
        else:
            url = ""

        if "entities" in entry[i].keys():
            keywords = list(set([k["name"] for k in entry[i]["entities"]]))
            keywords = "; ".join(keywords)
        else:
            keywords = ""


        rows.append([url, entry[i]["title"]["text"],
    entry[i]['paperAbstract']["text"], keywords, date, i_query])
    return(rows)

def format_zenodo_query(q):
    q = "+" + q.replace('AND ', "+")
    return(q)

def kw_lists(x, lkw):
    v = 0
    for kw in lkw:
        cnt = kw_list(x, kw)
        if cnt == 1:
            v = 1
    return(v)

def kw_in_text(x, kw):
    v= 0
    for k in kw:
        if x.count(k) > 0:
            v = 1
    return(v)

def kw_list(x, kw):
    cnt = 0
    for k in kw:
        if x.count(k) > 0:
            cnt = cnt + 1
    if cnt == len(kw):
        v = 1
    else:
        v= 0
    return(v)

def get_query_var(x, pl = True, a = True):
    l = [y.strip() for y in x.split("AND")]
   
    wvar = list()
    for w in l:
        if (pl and a) :
            wvar.append(list(set(list(["Québec" if w == "Quebec" else w, "Quebec" if w == "Québec" else w, get_plural(w)]))))
        elif (pl and not a):
            wvar.append(list(set(list([w, get_plural(w)]))))
        else :
            wvar.append(list(set(list(["Québec" if w == "Quebec" else w, "Quebec" if w == "Québec" else w]))))
    return([list(x) for x in list(itertools.product(*wvar))])   

def get_plural(sg):
    import inflect
    if sg[0].isupper():
        pl = sg
    elif sg[0] == '""':
        pl =sg
    else:
        engine = inflect.engine()
        pl = engine.plural(sg)
    return(pl)

def retrieve_semantic(queries, offset = 0, limit = 100, year_min = 1980, year_max = None):
    if year_max is not None:
        year = "{0}-{1}".format(year_min, year_max)
    else :
        year = "{0}-".format(year_min)
    
    rows = []
    delay_seconds = 5
    errors = []
    for i, query in queries.items():
        query_vars = get_query_var(query, pl = True, a = False)  # Generate queries variants (1) Québec/Quebec and (2) plural forms
        r = []
       
        for q in query_vars:
            time.sleep(delay_seconds)
            q = "+".join(q)
            response = requests.get("https://api.semanticscholar.org/graph/v1/paper/search?query={0}&offset={1}&limit={2}&year={3}".format(q, offset, limit, year))
            result = response.json()
            print("we are here")
            print("https://api.semanticscholar.org/graph/v1/paper/search?query={0}&offset={1}&limit={2}&year={3}".format(q, offset, limit, year))
            #result = dataList
            if 'total' in result:
                total = result["total"] # mostrar el total de las queries
                relevant_results = result["data"]
                # imprimir el total 
                print(f'Analyzed {total} results. Found {len(relevant_results)} relevant results')
                print("http://api.semanticscholar.org/graph/v1/paper/search?query={0}&offset={1}&limit={2}&year={3}".format(q, offset, limit, year))
                for entry in result["data"]:
                    time.sleep(delay_seconds)
                    content = requests.get("https://api.semanticscholar.org/graph/v1/paper/{0}?fields=url,externalIds,title,venue,year,abstract".format(entry["paperId"]))
                    res = content.json()
                    #res = entry
                    if 'paperId' in res:

                         print('*******paperId query:********')
                         print("https://api.semanticscholar.org/graph/v1/paper/{0}?fields=url,externalIds,title,venue,year,abstract".format(entry["paperId"]))
                         #print(res)
                         r.append(extract_semantic_scholar(res, i))
                    else:
                        print(f'error paperId: {entry["paperId"]}')   
                        print(res)   
                        errors.append({"paperId":entry["paperId"], "error": res})

                    
                   

            else:
                print("***** error ****")

            #print(result)
           # 
           
    #print("******* r ******")
    #print(r)
    rows = rows + r
    df = pd.DataFrame(rows)
    rows, cols = df.shape
    
    print("******* cols ******")
    print(cols)
    if cols == 7 :
        df.columns = ["doi", "url", "journal", "title", "description", "publication_date", "id_query"]
        df["source"] = "semantic_scholar"
        df['id_query'] = df['id_query'].astype(str)   
        df['id_query'] = df.groupby(['url'])['id_query'].transform(lambda x: ','.join(list(set(x))))
        df = df.drop_duplicates(subset = ["url"])

    # imprimir el total 
    print("****** df ******")
    print(df)
    return  df, errors

def retrieve_semantic_by_paperIds():
    return

def retrieve_zenodo(queries):
    rows = []
    counter = 0
    for i, query in queries.items():
        query_vars = get_query_var(query)  # Generate queries variants (1) Québec/Quebec and (2) plural forms
        r = []
        #print('query_vars')
        #print(query_vars)
        type = 'dataset'
        size = 1000
        access_token=  "Mf4LxV3d12BadrTyBke4vKphD6SO59ILOCHKGlQBbrcuKWMPlcUG51jBCA7p"
        for q in query_vars:
            q = "+"+" +".join(q)
            print("Searching for query...")
            print(q)
            #response = requests.get("https://zenodo.org/api/records?q={0}&type={1}&size={2}&access_token={3}".format(q, type, size, access_token))
            response = requests.get('https://zenodo.org/api/records',
                                params={'q': q,
                                        "type" : type, 
                                        "size":size,
                                        'access_token': access_token})
            
            #print(f"status code : {response.status_code}")
            if response.status_code == 200:
                print(len(response.json()["hits"]["hits"]))
                counter = counter + len(response.json()["hits"]["hits"])
                for j in range(0, len(response.json()["hits"]["hits"])):
                    entry = response.json()["hits"]["hits"][j]
                    r.append(extract_zenodo(entry, i)) 
                r = [list(x) for x in set(tuple(x) for x in r)]
                rows = rows + r
            else :
                print(f"there was an error fetching the data: status code : {response.status_code}")
    
    print(f" total of hits: {counter}")
    df = pd.DataFrame(rows)
    rows, cols = df.shape
    if cols == 10 :
        df.columns = ["url", "title", "description", "method", "notes", "keywords", "locations", "publication_date", "cited_articles", "id_query"]
        df["source"] = "zenodo"
        df['url'] = df['url'].apply(lambda row : "https://doi.org/" + row)
        df['id_query'] = df['id_query'].astype(str)   
        df['id_query'] = df.groupby(['url'])['id_query'].transform(lambda x: ','.join(list(set(x))))
        df = df.drop_duplicates(subset = ["url"])
    return(df)

def request_semantic(keyword, page=1, min_year=2018, max_year=2022):

        headers = {
            "Connection": "keep-alive",
            "sec-ch-ua": '"Google Chrome";v="95", "Chromium";v="95", ";Not A Brand";v="99"',
            "Cache-Control": "no-cache,no-store,must-revalidate,max-age=-1",
            "Content-Type": "application/json",
            "sec-ch-ua-mobile": "?1",
            "User-Agent": "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Mobile Safari/537.36",
            "X-S2-UI-Version": "20166f1745c44b856b4f85865c96d8406e69e24f",
            "sec-ch-ua-platform": '"Android"',
            "Accept": "*/*",
            "Origin": "https://www.semanticscholar.org",
            "Sec-Fetch-Site": "same-origin",
            "Sec-Fetch-Mode": "cors",
            "Sec-Fetch-Dest": "empty",
            #"Referer": "https://www.semanticscholar.org/search?year%5B0%5D=2018&year%5B1%5D=2022&q=multi%20label%20text%20classification&sort=relevance",
            "Accept-Language": "en-GB,en-US;q=0.9,en;q=0.8",
        }

        data = json.dumps(
            {
                "queryString": f"{keyword.lower()}",
                "page": page,
                "pageSize": 10,
                 "sort": "relevance",
                "authors": [],
                "coAuthors": [],
                "venues": [],
                "yearFilter": {"min": min_year, "max": max_year},
                "requireViewablePdf": False,
                "publicationTypes": [],
                "externalContentTypes": [],
                "fieldsOfStudy": [],
                "useFallbackRankerService": False,
                "useFallbackSearchCluster": True,
                "hydrateWithDdb": True,
                "includeTldrs": True,
                "performTitleMatch": True,
                "includeBadges": True,
                "tldrModelVersion": "v2.0.0",
                "getQuerySuggestions": False,
            }
        )

        response = requests.post(
            "https://www.semanticscholar.org/api/1/search", headers=headers, data=data
        )
        return response