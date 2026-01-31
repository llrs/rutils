# Use system's notify-send to send a message

Creates a pop up message on the system if it is run from an interactive
session.

## Usage

``` r
llrs_notify_system(..., title, message = NULL, icon = NULL, urgency = NULL)
```

## Arguments

- ...:

  Ignored arguments (to make it compatible with a pipe)

- title:

  Title of the message: character shorter than 40 characters.

- message:

  Message of the notification.

- icon:

  Icon, to pick from the system.

- urgency:

  Urgency of the system

## Value

Always TRUE (even if no notification can be created).

## Examples

``` r
llrs_notify_system(title = "Works", message = "My first message")
#> Warning: beep() could not play the sound due to the following error:
#> Error in play.default(x, rate, ...): no audio drivers are available
#> [1] TRUE
```
