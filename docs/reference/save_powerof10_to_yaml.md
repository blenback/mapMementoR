# Save Powerof10 athlete race data to YAML

Save Powerof10 athlete race data to YAML

## Usage

``` r
save_powerof10_to_yaml(
  first_name,
  surname,
  club,
  yaml_path = NULL,
  event = NULL,
  year = NULL
)
```

## Arguments

- first_name:

  Athlete's first name

- surname:

  Athlete's surname

- club:

  Athlete's club

- yaml_path:

  Path to save the YAML file (default: "\<first_name\>\_\_races.yaml")

- event:

  Optional vector of events to filter e.g "HM", "Mar", "5K "parkrun"
  etc.

- year:

  Optional vector of years to filter

## Value

Saves a YAML file with the athlete's race data
