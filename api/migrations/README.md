___

This following text was taken directly from my friend Arie's repository for a sick project
of his called [Code Tidbit](https://github.com/amilner42/code-tidbit/blob/master/backend/migrations/README.md).

# Migrations

Migrations are ways of applying changes to the database. All migrations should
start with a number (and be ordered) and have a meaningful name, ending in
`.migration.js`, eg. `1-init.migration.js`.

Migrations connect to the database through the `db` variable, eg `db.users`
would get you the users collection.

### Example Migration

```javascript
/**
 * The default expenditure categories to populate the database.
 */
const defaultExpenditureCategories = [
  {
    name: "Groceries",
    color: "#893302"
  },
  {
    name: "Rent",
    color: "#082932"
  },
  {
    name: "Casual Spending",
    color: "#ad0211"
  }
]


db.expenditureCategories.insert(defaultExpenditureCategories);
```

### Running a Migration

To run a migration such as the one above which initializes the database,
simply run:
`mongo localhost:27017/DB_NAME migration-name.migration.js`

___

The rest of this README was written by myself (Aly).

When working with a local db, it seems the command above can be simplified to:
`mongo DB_NAME migration-name.migration.js`

**Last run migration:** `5-archived-habit-field.migration.js` (up to date)
