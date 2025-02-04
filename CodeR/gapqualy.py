#Gap Qualy

# Enable cache (put your cache between the parenthesis)
fastf1.Cache.enable_cache()

# Initialize DataFrame
gapQ = pd.DataFrame(columns=['Driver','Team','Gap'])

year = 2024
gp = "Bahrain"
ses = 'Q'

# Get session data
session = fastf1.get_session(year, gp, ses)
session.load()
drivers = pd.unique(session.laps['Driver'])
list_fastest_laps = []

# Calculate fastest laps for each driver
for drv in drivers:
    drvs_fastest_lap = session.laps.pick_driver(drv).pick_fastest()
    list_fastest_laps.append(drvs_fastest_lap)

fastest_laps = fastf1.core.Laps(list_fastest_laps).sort_values(by='LapTime').reset_index(drop=True)
pole_lap = fastest_laps.iloc[0]  # Get the pole position lap

# Calculate and store gap in seconds
for index, row in fastest_laps.iterrows():
    gap = (row['LapTime'] - pole_lap['LapTime']).total_seconds()  # Calculate gap in seconds
    pos = {'Driver': row['Driver'],'Team': row['Team'], 'Gap': gap}
    gapQ = gapQ.append(pos, ignore_index=True)

# Print and save results
print(gapQ)


