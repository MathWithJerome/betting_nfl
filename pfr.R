webpage = read_html("https://www.pro-football-reference.com/years/2020/")

webpage %>% html_elements(xpath = '//*[@id="csv_team_stats"]')

div = webpage %>% html_elements('div')


divs = do.call(cbind,div)
