# SQLite storage backend (schema matches Python Optuna)

SQLite storage backend (schema matches Python Optuna)

## Usage

``` r
sqlite_storage(path)
```

## Arguments

- path:

  Path to SQLite file.

## Value

A `SqliteStorage` R6 object.

## Examples

``` r
if (FALSE) { # \dontrun{
storage <- sqlite_storage(tempfile(fileext = ".sqlite"))
study <- create_study(storage = storage, study_name = "my_study")
} # }
```
