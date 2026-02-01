# Send toot

Split and merge the text as needed to reduce the number of toots to
post. It only handles text and not images as per
[`rtoot::post_thread()`](https://gesistsa.github.io/rtoot/reference/post_thread.html)

## Usage

``` r
llrs_toots(msg, width = 500, join_text = ". ")
```

## Arguments

- msg:

  A vector of strings if possible they will be joined

- width:

  Allowed width on the server

- join_text:

  Character used to join the text

## Value

The ids of the toots posted

## Examples
