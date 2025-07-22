# ===================================================================
# ANALISIS STATISTIK DESKRIPTIF DATA KERENTANAN SOSIAL INDONESIA
# ===================================================================

# -------------------------------------------------------------------
## Bagian 1: Instalasi dan Memuat Library
# -------------------------------------------------------------------
# Skrip ini akan memeriksa apakah library yang dibutuhkan sudah terinstal.
# Jika belum, skrip akan menginstalnya secara otomatis.


  library(readxl)

  library(ggplot2)

  library(dplyr)

  library(tidyr)



# -------------------------------------------------------------------
## Bagian 2: Impor Dataset
# -------------------------------------------------------------------


sovi_data <- read_excel("C:/BERKAS STIS/TINGKAT 2/2KS3/SEMESTER 4/KOMSTAT/DASHBOARD UAS/sovi_data.xlsx")


# -------------------------------------------------------------------
## Bagian 3: Ringkasan Statistik (Summary Model)
# -------------------------------------------------------------------
# Menampilkan ringkasan statistik dasar untuk setiap variabel (kolom).
# Ini termasuk nilai minimum, maksimum, rata-rata, median, dan kuartil.
cat("=====================================\n")
cat("Struktur Data (str)\n")
cat("=====================================\n")
str(sovi_data)

cat("\n\n=====================================\n")
cat("Ringkasan Statistik (summary)\n")
cat("=====================================\n")
summary(sovi_data)


# -------------------------------------------------------------------
## Bagian 4: Visualisasi Data
# -------------------------------------------------------------------

### A. Boxplot untuk melihat sebaran data per variabel
# Boxplot sangat baik untuk mengidentifikasi sebaran, median, dan outlier.
# Contoh: Boxplot untuk variabel POVERTY (persentase kemiskinan)
ggplot(sovi_data, aes(y = POVERTY)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  labs(title = "Boxplot Sebaran Data Kemiskinan (POVERTY)",
       y = "Persentase Kemiskinan (%)",
       x = "") +
  theme_minimal()

# Untuk membuat boxplot bagi semua variabel numerik sekaligus (mirip Gambar 1 di jurnal)
# Kita akan mengubah format data dari 'wide' ke 'long'
sovi_data_long <- sovi_data %>%
  select(-DISTRICTCODE) %>% # Hapus kolom non-numerik untuk plot
  pivot_longer(cols = everything(), names_to = "Variabel", values_to = "Nilai")

ggplot(sovi_data_long, aes(x = Variabel, y = Nilai, fill = Variabel)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ Variabel, scales = "free_y") + # 'scales = "free_y"' agar setiap plot punya skala Y sendiri
  labs(title = "Boxplot Sebaran untuk Setiap Variabel Kerentanan Sosial",
       y = "Nilai (Persentase atau Jumlah)",
       x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Sembunyikan label di sumbu x
        axis.ticks.x = element_blank())


### B. Bar Chart untuk menampilkan peringkat
# Bar chart cocok untuk membandingkan nilai antar kategori.
# Contoh: 10 kabupaten dengan persentase kemiskinan (POVERTY) tertinggi.
top_10_poverty <- sovi_data %>%
  arrange(desc(POVERTY)) %>% # Urutkan dari yang terbesar
  slice_head(n = 10)         # Ambil 10 baris teratas

ggplot(top_10_poverty, aes(x = reorder(DISTRICTCODE, -POVERTY), y = POVERTY)) +
  geom_bar(stat = "identity", fill = "salmon", color = "darkred") +
  labs(title = "10 Kabupaten dengan Tingkat Kemiskinan Tertinggi",
       x = "Kode Kabupaten (DISTRICTCODE)",
       y = "Persentase Kemiskinan (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Miringkan label agar tidak tumpang tindih


### C. Scatter Plot untuk melihat hubungan antar variabel
# Scatter plot digunakan untuk melihat korelasi atau hubungan antara dua variabel numerik.
# Contoh: Hubungan antara Pendidikan Rendah (LOWEDU) dan Kemiskinan (POVERTY).
ggplot(sovi_data, aes(x = LOWEDU, y = POVERTY)) +
  geom_point(color = "dodgerblue", alpha = 0.6) + # Titik-titik data
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Garis regresi linear
  labs(title = "Hubungan antara Pendidikan Rendah dan Kemiskinan",
       x = "Persentase Pendidikan Rendah (LOWEDU) (%)",
       y = "Persentase Kemiskinan (POVERTY) (%)",
       caption = "Garis merah menunjukkan tren linear") +
  theme_minimal()

# ===================================================================
# Akhir dari Skrip
# ===================================================================