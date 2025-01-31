import pandas as pd

df = pd.read_csv("../../data/data.csv")

df = df.loc[:, ["DataValue", "LocationAbbr", "Topic", "YearEnd", "YearStart"]]

df = df[(df["YearEnd"].isin([2016])) & (df["YearStart"].isin([2016]))]

df["DataValue"] = pd.to_numeric(df["DataValue"], errors="coerce")

df = df.dropna()

df["Percent"] = df["DataValue"] / df.groupby("LocationAbbr")["DataValue"].transform(
    "sum"
)

df.to_csv("../../data/csv/py.csv", index=False)
