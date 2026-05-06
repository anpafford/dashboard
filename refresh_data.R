library(DBI)
library(RPostgres)
library(lubridate)

con <- dbConnect(
  Postgres(),
  dbname = "graph-node",
  host = "localhost",
  port = 5432,
  user = "graph-node",
  password = "let-me-in"
)

# Kiln query
production_by_kiln <- dbGetQuery(con, "
WITH production_series AS (
  SELECT
    ps.kiln_id AS entity_id,
    k.serial_number AS entity_name,
    p.name AS project_name,
    date_trunc('week', ps.survey_date) AS period_trunc,
    to_char(date_trunc('week', ps.survey_date), 'IYYY-\"W\"IW') AS period,
    ps.approval_status,
    COALESCE(SUM(COALESCE(ps.output_biochar_dry_kg, ps.output_biochar_kg)), 0) AS biochar_kg,
    COUNT(*) AS survey_count
  FROM poka_yoke.production_survey ps
  INNER JOIN poka_yoke.project p ON p.id = ps.project_id
  INNER JOIN poka_yoke.kiln k ON k.id = ps.kiln_id
  WHERE ps.kiln_id IS NOT NULL
    AND ps.deleted_at IS NULL
    AND p.deleted_at IS NULL
    AND k.deleted_at IS NULL
    AND p.name NOT IN ('Research Ghana', 'Training')
  GROUP BY ps.kiln_id, k.serial_number, p.name, date_trunc('week', ps.survey_date), ps.approval_status
)
SELECT *
FROM production_series
ORDER BY entity_name, period_trunc
")

# Coordinator kiln
production_by_coordinator <- dbGetQuery(con, "
WITH production_series AS (
  SELECT
    pr.coordinator_id AS entity_id,
    u.full_name AS entity_name,
    p.name AS project_name,
    date_trunc('week', ps.survey_date) AS period_trunc,
    to_char(date_trunc('week', ps.survey_date), 'IYYY-\"W\"IW') AS period,
    ps.approval_status,
    COALESCE(SUM(COALESCE(ps.output_biochar_dry_kg, ps.output_biochar_kg)), 0) AS biochar_kg,
    COUNT(*) AS survey_count
  FROM poka_yoke.production_survey ps
  INNER JOIN poka_yoke.project p ON p.id = ps.project_id
  INNER JOIN poka_yoke.producer pr ON pr.id = ps.producer_id
  INNER JOIN poka_yoke.project_user pu ON pu.id = pr.coordinator_id
  INNER JOIN poka_yoke.user u ON u.id = pu.user_id
  WHERE ps.deleted_at IS NULL
    AND p.deleted_at IS NULL
    AND pr.deleted_at IS NULL
    AND u.deleted_at IS NULL
    AND p.name NOT IN ('Research Ghana', 'Training')
  GROUP BY pr.coordinator_id, u.full_name, p.name, date_trunc('week', ps.survey_date), ps.approval_status
)
SELECT *
FROM production_series
ORDER BY entity_name, period_trunc
")

saveRDS(production_by_kiln, "production_by_kiln.rds")
saveRDS(production_by_coordinator, "production_by_coordinator.rds")

dbDisconnect(con)
