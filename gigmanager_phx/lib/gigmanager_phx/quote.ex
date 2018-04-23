defmodule GigmanagerPhx.Quote do
  use Ecto.Schema
  import Ecto.Changeset

  schema "quotes" do
    field :description, :string
    belongs_to :account, GigmanagerPhx.Account
  end

  @doc false
  def changeset(quote, attrs) do
    quote
    |> cast(attrs, [:description])
  end
end
