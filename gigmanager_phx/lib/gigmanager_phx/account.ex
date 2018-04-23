defmodule GigmanagerPhx.Account do
  use Ecto.Schema
  import Ecto.Changeset

  schema "accounts" do
    field :calendar_id, :string
    field :calendar_oauth_token, :string
    field :owner_email, :string

    has_many :quotes, GigmanagerPhx.Quote, on_replace: :delete

    timestamps()
  end

  @doc false
  def changeset(account, attrs) do
    account
    |> cast(attrs, [:owner_email, :calendar_oauth_token, :calendar_id])
    |> cast_assoc(:quotes)
    |> validate_required([:owner_email])
  end
end
