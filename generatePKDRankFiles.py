import pandas as pd


def compute_ranking(save_as, percent=True, voivodeship=True):

    df = pd.read_csv('xd.csv', dtype={'AdressTERC': str}, sep = ',')

    if voivodeship:
        df['AdressTERC'] = df['AdressTERC'].str[0:2]
    else:
        df['AdressTERC'] = df['AdressTERC'].str[0:4]


    counts = df.groupby(['AdressTERC']).size()
    print(counts)

    df = df.groupby(['PKDMainSection', 'AdressTERC']).size()
    print(df)

    if percent:
        df = (df / counts)
    df = df.to_frame('size').reset_index()
    print(df)

    df = df.sort_values(['PKDMainSection', 'size'], ascending=[True, False]).reset_index()
    print(df)

    it = 1
    temp = None
    for i, row in df.iterrows():
        if row['PKDMainSection'] == temp:
            df.at[i, 'size'] = it
            it = it + 1
        else:
            temp = row['PKDMainSection']
            it = 1
            df.at[i, 'size'] = it
            it = it + 1

    del df['index']
    print(df)

    df['size'] = df['size'].astype(int)
    df.to_csv(save_as, index=False)


compute_ranking('woj_percent.csv')
compute_ranking('woj_count.csv', percent=False)
compute_ranking('pow_percent.csv', voivodeship=False)
compute_ranking('pow_count.csv', percent=False, voivodeship=False)
