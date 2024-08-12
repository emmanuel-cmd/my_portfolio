# Attribute Data Operations

# Load required packages
library(sf)      # vector data package introduced in Chapter 2
library(terra)   # raster data package introduced in Chapter 2
library(dplyr)   # tidyverse package for data frame manipulation
library(spData)  # spatial data package introduced in Chapter 2

# Vector attribute manipulation
# Like dataframes, sf objects have one column per attribute variable and one row per
# observation or feature

# sf also provides generics that allow sf objects to behave like regular data frames, 
# as shown by printing the class’s methods:
methods(class = "sf") # methods for sf objects, first 12 shown

# Let's recap how to discover the basic properties of vector data objects
class(world) # it's an sf object and a (tidy) data frame
dim(world)   # it is a 2 dimensional object, with 177 rows and 11 columns

# st_drop_geometry keeps only the attribute data of an sf object
world_df = st_drop_geometry(world)
class(world_df)
ncol(world_df) # one column of geometry dropped

# Vector attribute subsetting
world[1:6, ]    # subset rows by position
world[, 1:3]    # subset columns by position
world[1:6, 1:3] # subset rows and columns by position
world[, c("name_long", "pop")] # columns by name
world[, c(T, T, F, F, F, F, F, T, T, F, F)] # by logical indices
#world[, 888] # an index representing a non-existent column

# Using logical vectors for subsetting
i_small = world$area_km2 < 10000    # nations whose surface area is smaller than 10kkm2
summary(i_small) # a logical vector
small_countries = world[i_small, ]

# A more concise command which omits the intermediary object generates the same results:
small_countries = world[world$area_km2 < 10000, ]

# subset() provides another way to achieve similar results:
small_countries = subset(world, area_km2 < 10000)

# Key dplyr functions for working with sf dataframes demonstrated below:
world1 = select(world, name_long, pop) # select only two columns
names(world1)

# all columns between name_long and pop (inclusive)
world2 = select(world, name_long:pop) # select a range of columns using :

# Remove specific columns with -operator
# all columns except subregion and area_km2 (inclusive)
world3 = select(world, -subregion, -area_km2)

# Subset and rename columns at the same time with the new_name=old_name syntax
world4 = select(world, name_long, population = pop)

# The command above is more concise than the base R equivalent which requires
# two lines of code:
world5 = world[, c("name_long", "pop")] # subset columns by name
names(world5)[names(world5) == "pop"] = "population" # rename column manually

# Most dplyr verbs return a dataframe but you can extract a single column with pull:
pull(world, pop)
world$pop
world[["pop"]]

# slice is the row equivalent of select
slice(world, 1:6) # select rows 1 to 6

# filter() is dplyr’s equivalent of base R’s subset() function
world7 = filter(world, area_km2 < 10000)  # countries with a small area
world7 = filter(world, lifeExp > 82)      # with high life expectancy

# Chaining commands with pipes
# |> is the native pipe which takes its name from the Unix pipe |
# Pipes enable expressive code in which output of previous code becomes input in the next code
world7 = world |>
  filter(continent == "Asia") |>
  select(name_long, continent) |>    # select columns
  slice(1:5)   # select rows

# An alternative to piped operations is nested function calls which are harder to read:
world8 = slice(
  select(
    filter(world, continent == "Asia"),
    name_long, continent), 1:5)

# Another alternative is to split the operation into multiple self contained lines
# This approach has the advantage of saving the intermediate results with distinct names
# for the purpose of later debugging
world9_filtered = filter(world, continent == "Asia")
world9_selected = select(world9_filtered, continent)
world9 = slice(world9_selected, 1:5)

# VECTOR ATTRIBUTE AGGREGATION
# An example of attribute aggregation is calculating the number of people per continent based on country-level data (one row per country). 
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world,
                       na.rm = TRUE)
class(world_agg1)

# aggregate is a generic function which means it behaves differently depending on its
# inputs. sf provides the method aggregate.sf() which is activated automatically when x is an sf object and a by argument is provided:
world_agg2 = aggregate(world["pop"], by = list(world$continent), FUN = sum, 
                       na.rm = TRUE)
class(world_agg2)
nrow(world_agg2) 

# group_by |> summarize is the dplyr equivalent of aggregate function
world_agg3 = world |>
  group_by(continent) |>
  summarize(pop = sum(pop, na.rm = TRUE))

# This approach has the advantage of flexibility, readability and control over the new
# column names
world_agg4  = world |> 
  group_by(continent) |> 
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n())

# These aggregating functions return sf objects with rows representing continents 
# and geometries containing the multiple polygons representing each land mass and 
# associated islands (this works thanks to the geometric operation ‘union’) 

# Let's combine what we have learned so far about dplyr functions
world_agg5 = world |> 
  st_drop_geometry() |>                      # drop the geometry for speed
  select(pop, continent, area_km2) |> # subset the columns of interest  
  group_by(continent) |>                     # group by continent and summarize:
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n()) |>
  mutate(Density = round(Pop / Area)) |>     # calculate population density
  slice_max(Pop, n = 3) |>                   # keep only the top 3
  arrange(desc(N))                           # arrange in order of n. countries

# The top 3 most populous continents ordered by number of countries.
world_agg5

# Vector attribute joining
# dplyr join functions work the same on data frames and sf objects, 
# the only important difference being the geometry list column. 
# The result of data joins can be either an sf or data.frame object.
world_coffee = left_join(world, coffee_data) #> Joining with `by = join_by(name_long)`
class(world_coffee)

# The input datasets share a key variable (name_long)
# therefore the join worked without needing to specify the by argument
names(world_coffee)

# Plot the newly added attribute
plot(world_coffee["coffee_production_2017"])

# In the majority of cases where variable names dont match, 
# use the by argument to specify the joining columns
coffee_renamed = rename(coffee_data, nm = name_long)
world_coffee2 = left_join(world, coffee_renamed, by = join_by(name_long == nm))

# What if we want to keep countries that have a match in the key variable?
# In this case, an inner join can be used:
world_coffee_inner = inner_join(world, coffee_data)

nrow(world_coffee_inner) # only 45 rows compared to 47 in coffee_data

# We can identify the rows that didn't match using the setdiff function
setdiff(coffee_data$name_long, world$name_long)

# uses a string matching (regex) function from the stringr package to 
# confirm what Congo, Dem. Rep. of should be.
drc = stringr::str_subset(world$name_long, "Dem*.+Congo")
drc

# Fix name and redo join
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = drc
world_coffee_match = inner_join(world, coffee_data) #> Joining with `by = join_by(name_long)`

nrow(world_coffee_match)

# Join starting with the nonspatial data n adding variables from sf object
# Result is tidyverse tibble - result of a join tends to match its first argument
coffee_world = left_join(coffee_data, world) #> Joining with `by = join_by(name_long)`
class(coffee_world)

# Often we would like to create a new column based on existing columns
# For example, we want to calculate popn density for each country
world_new = world # do not overwrite our original data
world_new$pop_dens = world_new$pop / world_new$area_km2

# Alternatively we can use dplyr functions
# mutate adds columns at the penultimate position in the sf object
# The difference between mutate() and transmute() is that the latter 
# drops all other existing columns (except for the sticky geometry column).
world_new2 = world |> 
  mutate(pop_dens = pop / area_km2)
# Demonstrating the use of transmute
world |> 
  transmute(pop_dens = pop / area_km2)

# Use unite to paste together existing columns
# Paste together the continent and region_un column into a new column called con_reg
world_unite = world |>
  tidyr::unite("con_reg", continent:region_un, sep = ":", remove = TRUE) # specify remove arg to drop original columns

# Use separate to split one column into multiple columns using either a regular expression
# or character positions
world_separate = world_unite |>
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")

# Use dplyr rename to rename columns and base R func setNames for similar things
world |> 
  rename(name = name_long)

# setNames changes all column names at once and requires a character vector with a name 
# matching each columns
new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world_new_names = world |>
  setNames(new_names)

# Each of the above attribute data operations preserve the geometry 
# We can drop geometry e.g. to speed up aggregation
world_data = world |> st_drop_geometry()
class(world_data)

# st_geometry(world_st) = NULL   # this also works but overwrites original object

# Manipulating raster objects
# In contrast to the vector data model which represents 
# points,lines and polygons as discrete entities in space, 
# raster data represents continous surfaces
elev = rast(nrows = 6, ncols = 6,
            xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
            vals = 1:36)
# Raster objects can also contain values of class factor or logical in R
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)

# The raster object stores the corresponding look-up table or 
# “Raster Attribute Table” (RAT) as a list of data frames
# Each element of this list is a raster layer
cats(grain)

# Use levels to retrieve or add new or replace existing factor levels
grain2 = grain # do not overwrite the original data
levels(grain2) = data.frame(value = c(0, 1, 2), wetness = c("wet", "moist", "dry"))
levels(grain2)

# Raster subsetting - done with the [ operator

# return the value at row 1, column 1
elev[1, 1]

# return the value at cell ID 1
elev[1]

# Subsetting a multilayered raster object
two_layers = c(grain, elev)
names(two_layers) = c("grain", "elev")
two_layers[1]

# Cell values can be overwritten in conjuncting with a subsetting operation
elev[1, 1] = 0
elev[]   # retrieves all values in the raster obj

# Multiple cells can be modified this way:
elev[1, c(1, 2)] = 0

# Replacing values of multilayered rasters can be done with a matrix with as many columns
# as layers and rows as replaceable cells
two_layers = c(grain, elev) 
two_layers[1] = cbind(c(1), c(4))
two_layers[]

# Get the standard deviation of the raster values 
global(elev, sd) 

# If you provide multilayered raster object, they will summarize each layer separately:
summary(c(elev, grain))
global(c(elev, grain), mean)

# Additionally the freq allows to get the frequency table of categorical values
freq(grain)

# Visualizing raster value statistics - can be done using boxplot, density, hist, pairs
hist(elev)

# Exercises

# Load necessary packages and data
library(sf)
library(dplyr)
library(terra)
library(spData)
data(us_states)
data(us_states_df)

# E1. Create a new object called us_states_name that contains only the NAME column 
# from the us_states object using either base R ([) or tidyverse (select()) syntax. 
# What is the class of the new object and what makes it geographic?
us_states_new = us_states[, "NAME"] #> us_states |> select(NAME)
class(us_states_new)

# E2. Select columns from the us_states object which contain population data. 
# Obtain the same result using a different command (bonus: try to find three ways of 
# obtaining the same result). Hint: try to use helper functions, such as contains or 
# matches from dplyr (see ?contains).

us_states |>
  select(contains("pop"))

us_states |>
  select(matches("pop"))

# E3. Find all states with the following characteristics (bonus find and plot them):
# + Belong to the Midwest region.
# + Belong to the West region, have an area below 250,000 km2 and in 2015 a population 
#   greater than 5,000,000 residents (hint: you may need to use the function units::set_units() or as.numeric()).
# + Belong to the South region, had an area larger than 150,000 km2 and a total population in 2015 larger than 7,000,000 residents.

# All states that belong to the Midwest region
us_states |>
  filter(REGION=="Midwest")
# All states that belong to the West region and have an area below 250k
us_states |>
  filter(REGION=="West" & as.numeric(AREA)<250000 & total_pop_15>5000000)
# Belong to the South region and had an area larger than 150k 
us_states |>
  filter(REGION=="South" & as.numeric(AREA)>150000 & total_pop_15>7000000)

# E4. What was the total population in 2015 in the us_states dataset? 
# What was the minimum and maximum total population in 2015?
sum(us_states$total_pop_15) # total popn in 2015
min(us_states$total_pop_15)
max(us_states$total_pop_15)

# E5. How many states are there in each region?
us_states |>
  group_by(REGION) |>
  summarise(no_of_states = n())

# E6. What was the minimum and maximum total population in 2015 in each region? 
# What was the total population in 2015 in each region?
us_states |>
  group_by(REGION) |>
  summarise(min_pop = min(total_pop_15), max_pop = max(total_pop_15), total_pop = sum(total_pop_15))

# E7. Add variables from us_states_df to us_states, and create a new object called us_states_stats. 
# What function did you use and why? Which variable is the key in both datasets? 
# What is the class of the new object?
us_states_stats = inner_join(us_states, us_states_df, by = join_by(NAME == state))

# E8. us_states_df has two more rows than us_states. How can you find them? 
# (hint: try to use the dplyr::anti_join() function)
anti_join(us_states_df, us_states, by = join_by(state==NAME))

# E9. What was the population density in 2015 in each state? 
# What was the population density in 2010 in each state?
us_states_popdens = us_states |>
  mutate(pop_dens_10 = total_pop_10/as.numeric(AREA), 
         pop_dens_15 = total_pop_15/as.numeric(AREA))

# Calculate percentage change of total popn density between the two years 
us_states_popdens = us_states_popdens |>
  mutate(perc_change = (pop_dens_15-pop_dens_10)/pop_dens_10 * 100)

plot(us_states_popdens["perc_change"])

# E11. Change the columns’ names in us_states to lowercase. 
# (Hint: helper functions - tolower() and colnames() may help.)
us_statesmine = us_states
colnames(us_statesmine) = tolower(names(us_statesmine))

# E12. Using us_states and us_states_df create a new object called us_states_sel. 
# The new object should have only two variables - median_income_15 and geometry. 
# Change the name of the median_income_15 column to Income.
us_states_sel = us_states_stats["median_income_15"]
names(us_states_sel)[1] = "Income"

# E13. Calculate the change in the number of residents 
# living below the poverty level between 2010 and 2015 for each state. 
# (Hint: See ?us_states_df for documentation on the poverty level columns.) 
# Bonus: Calculate the change in the percentage of residents living below the poverty level in each state.
us_states_df |>
  mutate(poverty_change=(poverty_level_15-poverty_level_10)/poverty_level_10)

# Calculate change in percentage of residents
percus_states = us_states_df |>
  mutate(percpovertylevel10 = poverty_level_10/sum(poverty_level_10)*100, 
         percpovertylevel15 = poverty_level_15/sum(poverty_level_15)*100)

percus_change = percus_states |>
  mutate(perc_change=(percpovertylevel15-percpovertylevel10)/percpovertylevel10)
  
percus_change = inner_join(us_states, percus_change, by=join_by(NAME==state))
plot(percus_change["perc_change"])

# E14. What was the minimum, average and maximum state’s number of people living 
# below the poverty line in 2015 for each region? Bonus: What is the region with 
# the largest increase in people living below the poverty line?
us_region_pov = inner_join(
  us_states_df[, c("state", "poverty_level_10","poverty_level_15")], 
  us_states[,c("NAME", "REGION")], 
  by=join_by(state==NAME)
)

us_region_pov |>
  group_by(REGION) |>
  summarise(min_pov15 = min(poverty_level_15), 
            avg_pov15 = mean(poverty_level_15), 
            max_pov15 = max(poverty_level_15)) 

# Region with the largest increase of people living the poverty line
us_region_povchg = us_region_pov |>
  mutate(povchnge = ((poverty_level_15 - poverty_level_10)/poverty_level_10))

us_region_povchg |>
  group_by(REGION) |>
  summarise(mxchg = max(povchnge))

# E15. Create a raster from scratch with nine rows and columns and a resolution of 0.5 decimal degrees (WGS84). 
# Fill it with random numbers. Extract the values of the four corner cells.
mxrast = rast(nrows = 9, ncols = 9, res = 0.5,
     xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
     vals = sample(seq(0, 1, 0.1), 81, replace = T))

# Extract the values of the four corner cells
mxrast[1:4, 1:4]

# What is the common class of our example raster grain
class(grain)

# Plot the histogram and boxplot of the dem.tif file
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
hist(dem)
boxplot(dem)
