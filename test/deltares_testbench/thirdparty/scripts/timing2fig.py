import pandas as pd
from matplotlib import pyplot as plt

df = pd.read_csv('timing_history.csv')
df['computation time'] = df['computation time'].astype('int')
models = df.groupby('model')

plt.figure(figsize=(35, 15))
plt.subplots_adjust(hspace=0.25)
plt.suptitle("Computation time of DIMR sets for acceptance models", fontsize=16, y=0.92)

# set number of columns
ncols = 3
# calculate number of rows
nrows = len(models) // ncols + (len(models) % ncols > 0)
n = 0
for model, df_model in models:
    ax = plt.subplot(nrows, ncols, n + 1)
    x=df_model['version'].to_list()
    y=df_model['computation time'].values
    ax.plot(x, y)
    plt.xticks(rotation=45, ha='right')
    ax.set_title(model)
    n += 1

plt.savefig('timings.png', bbox_inches='tight')