# lager_slack
Simple [Lager](https://github.com/erlang-lager/lager) backend for [Slack](https://slack.com) webhooks

# Config
```erlang
{lager, [
  {handlers, [
    {lager_slack_backend, [
      {uri, "https://hooks.slack.com/..."}, % Slack webhook (https://api.slack.com/incoming-webhooks)
      {metadata, [pid, module]},            % (optional) Lager metadata appended to message (default: module)
      {sign, "MyApp"},                      % (optional) Message sign to identify your errors (appears in message footer)
      {level, error}                        % (optional) Log level (default: critical)
    ]}
  ]}
]}
```
