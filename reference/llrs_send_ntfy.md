# Send a message to the NTFY address

Send a message to the NTFY address

## Usage

``` r
llrs_send_ntfy(message, title, ..., topic = NULL)
```

## Arguments

- message:

  Text to be sent.

- title:

  Title of the notification.

- ...:

  Other arguments used in
  [`ntfy::ntfy_send()`](https://jonocarroll.github.io/ntfy/reference/ntfy_send.html)

- topic:

  Send the message to the default NTFY environment variable:
  "NTFY_TOPIC". Specify topic if you don't use the default.

## Value

The default `httr` response.

## Examples
