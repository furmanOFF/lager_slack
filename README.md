# lager_slack
Simple [Lager](https://github.com/erlang-lager/lager) backend for [Slack](https://slack.com)

## Format
lager_slack buffers incoming logs and sends them in packs of 20 logs per Slack message (as attachments).

## Configuration
To use this backend you must provide it with a [Slack Webhook](https://api.slack.com/incoming-webhooks) link to specific channel.

```erlang
{lager, [
  {handlers, [
    {lager_slack_backend, [
      {uri, "https://hooks.slack.com/..."}, % (required) Webhook URI
      {metadata, [pid, module]},            % Lager metadata appended to message (default: module)
      {sign, "MyApp"},                      % Message sign to identify your errors (appears in message footer)
      {level, error},                       % Log level (default: critical)
      {timeout, 5000},                      % (optional) Slack submit timeout in ms (default: 5000)
      {threshold, 20}                       % (optional) Max message attachement count (default/recommended: 20)
    ]}
  ]}
]}
```
