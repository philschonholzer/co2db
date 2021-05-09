# About CO<sub>2</sub> Database

What are the CO<sub>2</sub> emissions of things.
How can we as a society fight the climate crisis if we hardly know the opponent (CO<sub>2</sub>).
With the CO<sub>2</sub> database I want to show the different CO<sub>2</sub> emitters and their emissions.
Everyone should be able to know and understand what causes how much CO<sub>2</sub>.

## Contribution

I would love for you to contribute to the CO<sub>2</sub> database. Either by contributing of emitters or by improving the database. You can find the project on Github.

## Deployment

The app is deployed on IHPCloud.

### DB

The database connection can be found on the IHPCloud settings page. To dump the production db use pg_dump with the settings from IHPCloud.

Get data from production db:

```bash
pg_dump --host=database-cloud.XXXXXXXX.eu-central-1.rds.amazonaws.com -U userInIHPCloudDbSettings -W databaseInIHPCloudDbSettings > dump.sql
```

Import locally (not tested yet):

```bash
psql --host=/PATH/TO/PROJECT/DIRECTORY/build/db app < dump.sql
```
