# PA 10: Exploring the Star Wars Universe
Instructions

``` r
#using this later to add a variable to our data
trilogies <- factor(
  c("Prequels: Episode I-III", 
    "Originals: Episode IV-VI", 
    "Sequels: Episode VII"), 
  levels = c("Prequels: Episode I-III", 
             "Originals: Episode IV-VI", 
             "Sequels: Episode VII"))
```

***This task is complex. It requires many different types of abilities.
Everyone will be good at some of these abilities but nobody will be good
at all of them. In order to produce the best product possible, you will
need to use the skills of each member of your group.***

<!-- The person who who is going the farthest from CSUMB this summer starts as the Developer (typing and listening to instructions from the Coder)!  -->

## Goals for the Activity

- Apply methods of to use lists and iteration (using `purrr`) to extract
  data from various non-tabular data sets.  
- Create new data sets through the cleaning, organization, and joining
  of data from various sources  
- Create visualizations to explore the data  
- May the force be with you!

**THROUGHOUT THE Activity** be sure to follow the Style Guide by doing
the following:

- load the appropriate packages at the beginning of the Rmarkdown  
- use proper spacing  
- name all code chunks  
- comment at least once in each code chunk to describe why you made your
  coding decisions  
- add appropriate labels to all graphic axes

## Review: Extracting Information from Different Data Sets

Here is information about the fist 7 Star Wars films:

``` r
View(sw_films) 
```

We are going to explore the data contained in several lists similar to
this one (and the previously explored `sw_people`), combining skills
from all of our previous R code learning experiences.

How do the following two codes compare?

``` r
sw_films[[4]][["title"]]
```

    [1] "Revenge of the Sith"

``` r
sw_films %>% pluck(4,"title")
```

    [1] "Revenge of the Sith"

> Insert Answer Here

Suppose we want to pull out just the titles as a character vector,
select the correct code (comment out the rest) to perform this action.
You may want to run each line of code one at a time (remember
`Ctrl + Enter` for Windows with your cursor on that line of code).

``` r
#comment out the incorrect codes
sw_films %>% map("title")
```

    [[1]]
    [1] "A New Hope"

    [[2]]
    [1] "Attack of the Clones"

    [[3]]
    [1] "The Phantom Menace"

    [[4]]
    [1] "Revenge of the Sith"

    [[5]]
    [1] "Return of the Jedi"

    [[6]]
    [1] "The Empire Strikes Back"

    [[7]]
    [1] "The Force Awakens"

``` r
sw_films %>% map_chr("title")
```

    [1] "A New Hope"              "Attack of the Clones"   
    [3] "The Phantom Menace"      "Revenge of the Sith"    
    [5] "Return of the Jedi"      "The Empire Strikes Back"
    [7] "The Force Awakens"      

``` r
sw_films %>% map_dfc("title")
```

    New names:
    • `` -> `...1`
    • `` -> `...2`
    • `` -> `...3`
    • `` -> `...4`
    • `` -> `...5`
    • `` -> `...6`
    • `` -> `...7`

    # A tibble: 1 × 7
      ...1       ...2                 ...3               ...4      ...5  ...6  ...7 
      <chr>      <chr>                <chr>              <chr>     <chr> <chr> <chr>
    1 A New Hope Attack of the Clones The Phantom Menace Revenge … Retu… The … The …

Suppose we want to apply a function to count the number of specific
kinds of ships and vehicles in our data

Notice that for each film, the “starships” vector contains links to
information on those starships (though note this data is out of date and
should be linked at swapi.dev, not swapi.co).

``` r
sw_films[[1]][["starships"]]
```

    [1] "http://swapi.co/api/starships/2/"  "http://swapi.co/api/starships/3/" 
    [3] "http://swapi.co/api/starships/5/"  "http://swapi.co/api/starships/9/" 
    [5] "http://swapi.co/api/starships/10/" "http://swapi.co/api/starships/11/"
    [7] "http://swapi.co/api/starships/12/" "http://swapi.co/api/starships/13/"

So if we can count the number of webpage links that would tell us the
number of starships that appear in that movie. Here are three different
ways to count the number of urls under `starships`. Can you think of
another? (it is ok if you can’t). Compare and contrast how the three
codes work differently to do the same thing.

``` r
sw_films %>% map(., "starships") %>% map_dbl(~length(.))
```

    [1]  8  9  5 12 12  9  2

``` r
map_dbl(sw_films, ~length(.x$starships))
```

    [1]  8  9  5 12 12  9  2

``` r
sw_films %>% map_dbl(., ~length(.x$starships))
```

    [1]  8  9  5 12 12  9  2

> Insert Answer

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

## Part 1: Evaluating Hyperdrive in the Star Wars Episodes

We will use the third method from the previous section to extract out
the information we want from `sw_films`. For each row, specify if we
should use a regular `map()`, `map_dbl()`, or `map_chr()`.

**NOTE** Sometimes code like this gets a little finicky in R if you try
to run it with `Ctrl + Enter`. Instead, use the code chunk green arrow
to run the whole code chunk or highlight all of the code and then use
the shortcut to run it.

``` r
sw_ships_1 <- sw_films %>% {
  tibble(
    title = map____(., "title"), #character
    episode = map____(., "episode_id"), #numeric
    starships = map____(., ~length(.x$starships)), #numeric
    vehicles = map____(., ~length(.x$vehicles)), #numeric
    planets = map____(., ~length(.x$planets)) #numeric
  )}
```

    Error in map____(., "title"): could not find function "map____"

``` r
sw_ships_1
```

    Error: object 'sw_ships_1' not found

Let’s do a bit more data cleaning to 1) assign the Trilogy
classification to each episode, 2) calculate the total number of
starships (which have hyperdrive) and vehicles (which do not have
hyperdrive), and 3) calculate the proportion of total ships that have
hyperdrive. Fill in the missing codes.

``` r
sw_ships <- sw_ships_1 ____  
  #create a new variable called trilogy
  ____(trilogy = case_when(episode %in% 1:3 ~ trilogies[1],
                             episode %in% 4:6 ~ trilogies[2],
                             episode %in% 7 ~ trilogies[3])) %>% 
  #create a new variable called total_ships which adds vehicles and starships together
  ____(total_ships = vehicles + starships) %>%  
  #create a new variable called prop that calculate the percent hyperdrive
  ____(prop = starships / total_ships * 100) 
```

    Error in parse(text = input): <text>:1:24: unexpected input
    1: sw_ships <- sw_ships_1 _
                               ^

### Hyperdrive Use Across Films

Now, let’s make a plot examining how often hyperdrive ships appear in
each episode. Fill in the blanks withe appropriate functions.

``` r
sw_ships %>% 
  #be sure to order titles by order/episode
  ggplot(aes(y = ____(title, desc(episode)), 
             x = prop)) + 
  #we want bars but our data is already summarized!
  geom_____(aes(fill = trilogy)) + 
  labs(
    title = "The Rise of Hyperdrive",
    subtitle = "Percentage of Ships with Hyperdrive Capability"
  ) +
  #you may need to install `scales` package if you haven't already
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal() +
  #what aesthetic do we modify to change the bar color
  scale______viridis_d(end = 0.8) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
```

    Error in parse(text = input): <text>:3:19: unexpected input
    2:   #be sure to order titles by order/episode
    3:   ggplot(aes(y = __
                         ^

#### Canvas Quiz Question 1

Which movie has the second highest percentage of Hyperdrive ships?

> Insert Answer Here

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

### Hyperdrive Prevalence within the Universe

We can also look at a plot to see if there is a correlation between the
total number of ships and the number with hyperdrive (starships). Fill
in the blanks withe appropriate functions.

``` r
sw_ships %>% 
  ggplot(aes(x = total_ships, 
             y = starships)) +
  #make points
  geom_____(aes(color = trilogy)) +
  #fit a model
  geom_____(method = "lm") +
  #what does geom_text() do?
  geom_text(aes(label = title), 
            vjust = -1, 
            hjust = "inward", 
            size = 2.75) +
  labs(title = "Hyperdrive Correlations",
       subtitle = "The Number of Ships with Hyperdrive vs Total Ships") +
  theme_minimal() +
  #what aesthetic do we want to modify the color of points?
  scale______viridis_d(end = 0.8) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) 
```

    Error: object 'sw_ships' not found

#### Canvas Quiz Question 2

What do you notice about the use of hyperdrive type vehicles in the
episodes?

> Insert Answer

## Part 2: The Physical Features of Star Wars Characters

Recall the data for “people” in Star Wars:

``` r
View(sw_people)
```

We want to extract out `name`, `height`, and `mass` as `character`
vectors (for now, we have to deal with some issues in height and weight
later to change them into double type vectors) and keep `films` as a
list for now. Fill in the correct `map` type functions for each one.

``` r
sw_peeps <- sw_people %>%  {
  tibble(
    name = ____(., "name"),  #character
    height = ____(., "height"), #character
    mass = ____(., "mass"), #character
    films = map____(., "films") #list
  )}
sw_peeps
```

    Error in parse(text = input): <text>:3:13: unexpected input
    2:   tibble(
    3:     name = __
                   ^

Notice that the `films` column contains lists of urls for each film
reference. Let’s pull out that same information from the `sw_films` data
to have the `title` of the episode and the `url` as a `character`
vector, and the episode number as a numeric value. Fill in the correct
`map` type functions.

``` r
film_names <- sw_films %>% {
  tibble(
    episode_id = ____(., "episode_id"), #double
    episode_name = ____(., "title"), #character
    url = ____(., "url") #character
  )}
film_names
```

    Error in parse(text = input): <text>:3:19: unexpected input
    2:   tibble(
    3:     episode_id = __
                         ^

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

Now we can finish cleaning up our data by doing the following:

1)  turn `height` and `mass` into numeric vectors;  
2)  match the `films`/`urls` to their `episode_names` and assign that
    back to `sw_peeps`.

``` r
sw_peeps2 <- sw_peeps %>% 
  #use a function from readr to extract the numbers and replace "unknown" with na
  mutate(height = parse_____(height, na = "unknown"),
         mass = parse_____(mass, na = "unknown")) %>%
  #unnest the lists in films
  ____(cols = c("films")) %>% 
  #join the film data with episodes names to the people data
  _____join(film_names, by = c("films" = "url")) %>% 
  #remove the `films` url from the data frame
  ____(-films) %>% 
  #add the variable trilogy
  ____(trilogy = case_when(episode_id %in% 1:3 ~ trilogies[1],
                           episode_id %in% 4:6 ~ trilogies[2],
                           episode_id %in% 7   ~ trilogies[3]))
sw_peeps2
```

    Error in parse(text = input): <text>:6:4: unexpected input
    5:   #unnest the lists in films
    6:   __
          ^

### Size of Characters in the Star Wars Universe

We can now create a plot of height and mass by trilogy group to see if
the physique of characters differed across Trilogies (keeping in mind
the third set of Trilogies is incomplete in this data set).

``` r
sw_peeps2 %>% 
  filter(name != "Jabba Desilijic Tiure") %>% #major outlier removed
  #map the correct aesthetics
  ggplot(aes(x = ____, 
             y = ____, 
             color = ____))+
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Height of Character (cm)",
       y = "Mass of Character (kg)",
       color = "Trilogy Group",
       title = "Character Characteristics in Star Wars") +
  theme_minimal() +
  scale_color_viridis_d(end = 0.8) 
```

    Error in parse(text = input): <text>:4:19: unexpected input
    3:   #map the correct aesthetics
    4:   ggplot(aes(x = __
                         ^

#### Canvas Quiz Question 3

Write some code to identify who is is the heaviest (look at the graph to
help guide this) Star Wars characer (excluding Jabba Desilijic Tiure).

> Insert Answer Here

**REMEMBER TO RENDER YOUR FINAL DOCUMENT**

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

## OPTIONAL CHALLENGE PROBLEM

Your professor wants to use `purrr` to try and generate a height and
mass scatterplot for each episode, but I don’t want to type out all that
code. Here is where I got so far, but I am not convince this is the most
sophisticated or effective way to do this. Do some research and see if
you can find a way to put this process into production!

``` r
plots_sw <- sw_peeps %>% 
  nest(data = !episode_name) %>% 
  mutate(plot = map(data, ~ggplot(., aes(y=mass, x=height)) + 
                      geom_point() + 
                      geom_smooth(method = "lm", se = FALSE) + 
                      labs(title = paste0(episode_name))))
```

    Error: object 'sw_peeps' not found

``` r
print(plots_sw$plot)
```

    Error: object 'plots_sw' not found
