# Create a file-based journal storage backend

Create a file-based journal storage backend

## Usage

``` r
journal_storage(path)
```

## Arguments

- path:

  Path to the journal file (.jsonl).

## Value

A `JournalStorage` R6 object.

## Examples

``` r
if (FALSE) { # \dontrun{
storage <- journal_storage(tempfile(fileext = ".jsonl"))
study   <- create_study(storage = storage, study_name = "my_study")
} # }
```
