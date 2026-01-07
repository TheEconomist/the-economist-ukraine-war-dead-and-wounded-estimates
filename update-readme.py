#!/usr/bin/env python3
import pandas as pd
import math

def round_k(x):
    # round to nearest 1 000
    return int(round(x / 1_000.0) * 1_000)

# Paths (adjust if you’ve nested differently)
DEATHS_CSV    = "output-data/tracker/meta-estimate-deaths.csv"
CASUAL_CSV    = "output-data/tracker/meta-estimate-casualties.csv"
README_PATH   = "README.md"

# Load & parse dates
deaths = pd.read_csv(DEATHS_CSV, parse_dates=["date"])
casual = pd.read_csv(CASUAL_CSV, parse_dates=["date"])

def latest_valid(df):
    max_date = df["date"].max()
    cutoff = max_date - pd.Timedelta(days=7)
    df_recent = df[df["date"] >= cutoff]
    df_valid = df_recent.dropna(subset=["estimate", "pi_low", "pi_high"])
    if df_valid.empty:
        raise ValueError("No valid rows found within the last 7 days with estimate/pi bounds.")
    return df_valid.loc[df_valid.date.idxmax()]

# Pick latest rows with valid values
d = latest_valid(deaths)
c = latest_valid(casual)

# Format values
date_str = d.date.strftime("%Y-%m-%d")
d_lo  = round_k(d.pi_low)
d_hi  = round_k(d.pi_high)
d_est = round_k(d.estimate)

c_lo  = round_k(c.pi_low)
c_hi  = round_k(c.pi_high)
c_est = round_k(c.estimate)

# Build the snippet
snippet = f"""\
## Live estimates

As of **{date_str}**:
- Estimated deaths: **{d_lo:,}** to **{d_hi:,}** (or, roughly **{d_est:,}**)
- Estimated casualties: **{c_lo:,}** to **{c_hi:,}** (or, roughly **{c_est:,}**)
"""

# Inject between markers in README.md
start_marker = "<!-- ESTIMATES-START -->"
end_marker   = "<!-- ESTIMATES-END -->"

with open(README_PATH, "r", encoding="utf-8") as f:
    content = f.read()

before, _, rest = content.partition(start_marker)
_, _, after = rest.partition(end_marker)

new_md = before + start_marker + "\n\n" + snippet + "\n" + end_marker + after

with open(README_PATH, "w", encoding="utf-8") as f:
    f.write(new_md)

print("✅ README.md updated with latest estimates.")
