    print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia"] == 67.85
    print $ averageBalanceOfCities (accounts1, people1) ["Burgas","Plovdiv"] == 23.62
    print $ averageBalanceOfCities (accounts1,people1) ["Pleven", "Burgas", "Sofia","Gabrovo","Stara Zagora"] == 39.25
    print $ averageBalanceOfCities (accounts1, people1) ["Sofia", "Gabrovo", "Burgas"] == 39.25