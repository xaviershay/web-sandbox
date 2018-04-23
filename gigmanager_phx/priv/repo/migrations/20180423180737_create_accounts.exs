defmodule GigmanagerPhx.Repo.Migrations.CreateAccounts do
  use Ecto.Migration

  def change do
    create table(:accounts) do
      add :owner_email, :string, null: false
      add :calendar_oauth_token, :string
      add :calendar_id, :string

      timestamps()
    end

    create table(:quotes) do
      add :description, :string, null: false

      add :account_id, references(:accounts)
    end

  end
end
