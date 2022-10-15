import pandas as pd



# Function calculates name variation rate within a cluster

#input: merged-dataframe with at least first name, last name, and unique id
#input: first_name-column name of the first name
#input: last_name-column name is the last name
#input: unique_id-column name of the unique id

#output: A list of results, first element is the name variation rate, the second is the original df with name_var

def get_name_var(merged, first_name = 'name_first', last_name = 'name_last', unique_id= 'unique_id'):
    merged_cop = merged.copy()
    merged_cop['name'] = merged[first_name] + ' ' + merged[last_name]
    name_var = merged_cop.groupby([unique_id])['name'].nunique().reset_index(drop=True)
    final_stat =  sum(name_var>1)/len(name_var)
    
    cond = merged_cop.groupby([unique_id])['name'].nunique().reset_index().rename(columns={"name": 'var_count'}).assign(is_var=lambda df: (df['var_count'] >1).astype(int))
    
    #0 variation count means there is only one name in cluster
    cond['var_count'] = cond['var_count']-1
    
    return [final_stat,merged_cop.merge(cond, on='unique_id', how='left')]
    


########################################################################################################################


##helper function for homonomy rate
def helper_function(row, merged_cop):
    id_cond = merged_cop.unique_id != row['unique_id']
    name_cond = merged_cop.name == row['name']
    cond = sum(id_cond & name_cond)
    if cond >0:
        return 1
    return 0


# Function calculates homonomy rate across different clusters

#input: merged-dataframe with at least first name, last name, and unique id
#input: first_name-column name of the first name
#input: last_name-column name is the last name
#input: unique_id-column name of the unique id

# output: A list of results, first element is the homonomy rate, and the second is the dataframe with homonomy as a column

def get_homonomy_rate(merged, first_name = 'name_first', last_name = 'name_last', unique_id= 'unique_id'):
    merged_cop = merged.copy()
    merged_cop['name'] = merged[first_name] + ' ' + merged[last_name]
    
    #create harmony column
    merged_cop['homonomy'] = merged_cop.apply(helper_function,1, args=(merged_cop,))
   
    
    #return actual homonomy rate
    return [sum(merged_cop.groupby(['unique_id'])['homonomy'].max())/merged_cop['unique_id'].nunique(),merged_cop]

