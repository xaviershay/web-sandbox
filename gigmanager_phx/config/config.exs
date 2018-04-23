# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :gigmanager_phx,
  ecto_repos: [GigmanagerPhx.Repo]

# Configures the endpoint
config :gigmanager_phx, GigmanagerPhxWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "7807Yyizdqv3i02xYPwBIL7xIxhDlbBdbjta3+DDzuDBCTDhjULYyRUeD0c3o/Yz",
  render_errors: [view: GigmanagerPhxWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: GigmanagerPhx.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
