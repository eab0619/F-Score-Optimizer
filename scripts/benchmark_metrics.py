import pandas as pd



# Function calculates name variation rate within a cluster

#input: merged-dataframe with at least first name, last name, and unique id
#input: first_name-column name of the first name
#input: last_name-column name is the last name
#input: unique_id-column name of the unique id

def get_name_var(merged, first_name = 'name_first', last_name = 'name_last', unique_id= 'unique_id'):
    merged_cop = merged.copy()
    merged_cop['name'] = merged[first_name] + ' ' + merged[last_name]
    name_var = merged_cop.groupby([unique_id])['name'].nunique().reset_index(drop=True)
    return sum(name_var>1)/len(name_var)


########################################################################################################################


##helper function for harmony rate
def helper_function(row, merged_cop):
    id_cond = merged_cop.unique_id != row['unique_id']
    name_cond = merged_cop.name == row['name']
    cond = sum(id_cond & name_cond)
    if cond >0:
        return 1
    return 0


# Function calculates harmony rate across different clusters

#input: merged-dataframe with at least first name, last name, and unique id
#input: first_name-column name of the first name
#input: last_name-column name is the last name
#input: unique_id-column name of the unique id

def get_harmony_rate(merged, first_name = 'name_first', last_name = 'name_last', unique_id= 'unique_id'):
    merged_cop = merged.copy()
    merged_cop['name'] = merged[first_name] + ' ' + merged[last_name]
    
    #create harmony column
    merged_cop['harmony'] = merged_cop.apply(helper_function,1, args=(merged_cop,))
   
        
    #harmony rate dataset
    #return merged_cop
    
    #return actual harmony rate
    return sum(merged_cop.groupby(['unique_id'])['harmony'].max())/merged_cop['unique_id'].nunique()
