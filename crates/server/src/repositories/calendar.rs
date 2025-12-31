use sqlx::SqlitePool;

/// Common SELECT fields for calendar queries
const SELECT_CALENDAR: &str = r#"
    SELECT
        bgmtv_id, subject_type, name, name_cn, summary,
        air_date, air_weekday,
        rating_total, rating_score, rank,
        collection_doing, images_json, updated_at
    FROM calendar_subject
"#;

pub struct CalendarRepository;

impl CalendarRepository {
    /// Batch upsert calendar subjects (transaction for atomicity)
    pub async fn upsert_batch(
        pool: &SqlitePool,
        subjects: &[CalendarSubjectRow],
    ) -> Result<(), sqlx::Error> {
        let mut tx = pool.begin().await?;

        for subject in subjects {
            sqlx::query(
                r#"
                INSERT INTO calendar_subject (
                    bgmtv_id, subject_type, name, name_cn, summary,
                    air_date, air_weekday,
                    rating_total, rating_score, rank,
                    collection_doing, images_json
                )
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
                ON CONFLICT(bgmtv_id) DO UPDATE SET
                    subject_type = excluded.subject_type,
                    name = excluded.name,
                    name_cn = excluded.name_cn,
                    summary = excluded.summary,
                    air_date = excluded.air_date,
                    air_weekday = excluded.air_weekday,
                    rating_total = excluded.rating_total,
                    rating_score = excluded.rating_score,
                    rank = excluded.rank,
                    collection_doing = excluded.collection_doing,
                    images_json = excluded.images_json,
                    updated_at = CURRENT_TIMESTAMP
                "#,
            )
            .bind(subject.bgmtv_id)
            .bind(subject.subject_type)
            .bind(&subject.name)
            .bind(&subject.name_cn)
            .bind(&subject.summary)
            .bind(&subject.air_date)
            .bind(subject.air_weekday)
            .bind(subject.rating_total)
            .bind(subject.rating_score)
            .bind(subject.rank)
            .bind(subject.collection_doing)
            .bind(&subject.images_json)
            .execute(&mut *tx)
            .await?;
        }

        tx.commit().await?;
        Ok(())
    }

    /// Get all calendar subjects ordered by weekday and collection count
    pub async fn get_all(pool: &SqlitePool) -> Result<Vec<CalendarSubjectRow>, sqlx::Error> {
        let query = format!(
            "{} ORDER BY air_weekday, collection_doing DESC",
            SELECT_CALENDAR
        );
        let rows = sqlx::query_as::<_, CalendarSubjectRow>(&query)
            .fetch_all(pool)
            .await?;

        Ok(rows)
    }

    /// Get calendar subjects by weekday
    pub async fn get_by_weekday(
        pool: &SqlitePool,
        weekday: i32,
    ) -> Result<Vec<CalendarSubjectRow>, sqlx::Error> {
        let query = format!(
            "{} WHERE air_weekday = $1 ORDER BY collection_doing DESC",
            SELECT_CALENDAR
        );
        let rows = sqlx::query_as::<_, CalendarSubjectRow>(&query)
            .bind(weekday)
            .fetch_all(pool)
            .await?;

        Ok(rows)
    }

    /// Check if any calendar data exists
    pub async fn has_data(pool: &SqlitePool) -> Result<bool, sqlx::Error> {
        let count: (i64,) = sqlx::query_as("SELECT COUNT(*) FROM calendar_subject")
            .fetch_one(pool)
            .await?;
        Ok(count.0 > 0)
    }

    /// Delete all calendar subjects (for full refresh)
    pub async fn delete_all(pool: &SqlitePool) -> Result<u64, sqlx::Error> {
        let result = sqlx::query("DELETE FROM calendar_subject")
            .execute(pool)
            .await?;

        Ok(result.rows_affected())
    }

    /// Delete subjects not in the provided list of bgmtv_ids
    pub async fn delete_except(pool: &SqlitePool, keep_ids: &[i64]) -> Result<u64, sqlx::Error> {
        if keep_ids.is_empty() {
            return Self::delete_all(pool).await;
        }

        let placeholders = keep_ids
            .iter()
            .enumerate()
            .map(|(i, _)| format!("${}", i + 1))
            .collect::<Vec<_>>()
            .join(",");

        let query = format!(
            "DELETE FROM calendar_subject WHERE bgmtv_id NOT IN ({})",
            placeholders
        );

        let mut q = sqlx::query(&query);
        for id in keep_ids {
            q = q.bind(id);
        }

        let result = q.execute(pool).await?;
        Ok(result.rows_affected())
    }
}

/// Internal row type for mapping SQLite results
#[derive(Debug, Clone, sqlx::FromRow)]
pub struct CalendarSubjectRow {
    pub bgmtv_id: i64,
    pub subject_type: i32,
    pub name: String,
    pub name_cn: String,
    pub summary: String,
    pub air_date: String,
    pub air_weekday: i32,
    pub rating_total: Option<i64>,
    pub rating_score: Option<f64>,
    pub rank: Option<i64>,
    pub collection_doing: i64,
    pub images_json: String,
    pub updated_at: String,
}

impl CalendarSubjectRow {
    /// Create a row from BGM.tv CalendarSubject
    pub fn from_bgmtv(subject: &bgmtv::CalendarSubject) -> Self {
        let images_json =
            serde_json::to_string(&subject.images).unwrap_or_else(|_| "{}".to_string());

        Self {
            bgmtv_id: subject.id,
            subject_type: subject.subject_type as i32,
            name: subject.name.clone(),
            name_cn: subject.name_cn.clone(),
            summary: subject.summary.clone(),
            air_date: subject.air_date.clone(),
            air_weekday: subject.air_weekday,
            rating_total: subject.rating.as_ref().map(|r| r.total),
            rating_score: subject.rating.as_ref().map(|r| r.score),
            rank: subject.rank,
            collection_doing: subject.collection.as_ref().map(|c| c.doing).unwrap_or(0),
            images_json,
            updated_at: String::new(), // Will be set by database
        }
    }

    /// Convert to BGM.tv CalendarSubject for API response
    pub fn to_bgmtv(&self) -> bgmtv::CalendarSubject {
        let images: bgmtv::SubjectImages =
            serde_json::from_str(&self.images_json).unwrap_or_else(|e| {
                tracing::warn!(
                    "Failed to parse images_json for bgmtv_id {}: {}",
                    self.bgmtv_id,
                    e
                );
                bgmtv::SubjectImages {
                    small: String::new(),
                    grid: String::new(),
                    large: String::new(),
                    medium: String::new(),
                    common: String::new(),
                }
            });

        let rating = match (self.rating_total, self.rating_score) {
            (Some(total), Some(score)) => Some(bgmtv::CalendarRating { total, score }),
            _ => None,
        };

        let collection = Some(bgmtv::CalendarCollection {
            doing: self.collection_doing,
        });

        bgmtv::CalendarSubject {
            id: self.bgmtv_id,
            subject_type: match self.subject_type {
                1 => bgmtv::SubjectType::Book,
                2 => bgmtv::SubjectType::Anime,
                3 => bgmtv::SubjectType::Music,
                4 => bgmtv::SubjectType::Game,
                6 => bgmtv::SubjectType::Real,
                unknown => {
                    tracing::warn!(
                        "Unknown subject_type {} for bgmtv_id {}, using default",
                        unknown,
                        self.bgmtv_id
                    );
                    bgmtv::SubjectType::default()
                }
            },
            name: self.name.clone(),
            name_cn: self.name_cn.clone(),
            summary: self.summary.clone(),
            air_date: self.air_date.clone(),
            air_weekday: self.air_weekday,
            rating,
            rank: self.rank,
            images,
            collection,
        }
    }
}
