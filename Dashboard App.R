# =================================================================
##          DASHBOARD ANALISIS KERENTANAN SOSIAL INDONESIA           
## =================================================================
## 1. MEMUAT LIBRARY YANG DIBUTUHKAN
# -----------------------------------------------------------------
library(MASSTIMATE)
library(lmtest)
library(moments)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)
library(sf)
library(car)
library(nortest)
library(DT)
library(tidyr)
library(EnvStats)
library(agricolae)
library(rmarkdown)
library(officer)
library(knitr)
library(xml2)
library(webshot2)

# Package Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# =================================================================
##          REVISI FINAL: PEMISAHAN DATA UNTUK KLASTER PETA          
## =================================================================
## 1. Muat Data untuk Fitur-Fitur Umum (NON-PETA)
# -----------------------------------------------------------------
sovi_data <- read_excel("sovi_data.xlsx")

# Lakukan pembersihan dan konversi numerik pada sovi_data
sovi_data <- sovi_data %>%
  mutate(
    across(
      c("FEMALE", "POPULATION"), 
      ~ as.numeric(gsub("\\D", "", .))
    )
  )

# 2. Siapkan Daftar Variabel untuk Dropdown di seluruh aplikasi (dari sovi_data)
# -----------------------------------------------------------------
all_vars <- names(sovi_data)
vars_for_dropdowns <- all_vars[!all_vars %in% c("NO", "DISTRICTCODE")]
vars_for_assumption <- vars_for_dropdowns[!vars_for_dropdowns %in% "POPULATION"]
vars_for_eksplorasi <- vars_for_dropdowns
vars_for_kategori_dropdown <- vars_for_dropdowns

# 3. Muat & Proses Data KHUSUS UNTUK KLASTER PETA
# -----------------------------------------------------------------
# Langsung muat file GeoJSON
data_peta_raw <- st_read("indonesia511.geojson")

# Fungsi normalisasi
normalize <- function(x) {
  return ((x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T)))
}

# Identifikasi variabel untuk skor dengan cara mengecualikan kolom ID dan geometri
exclude_cols <- c("FID", "gid", "kdkab", "kdprov", "nmkab", "nmprov", "kodeprkab", "geometry")
vars_for_score <- names(data_peta_raw)[!names(data_peta_raw) %in% exclude_cols]

# Hapus baris NA dari data_peta_raw sebelum analisis (LANGKAH INI PENTING DAN JANGAN DIHAPUS)
data_peta_clean <- data_peta_raw %>%
  na.omit()

# Normalisasi semua variabel skor pada data peta yang sudah bersih
data_peta_normalized <- data_peta_clean %>%
  mutate(across(all_of(vars_for_score), normalize))

# Hitung vektor Skor Kerentanan dari data peta
skor_vector <- data_peta_normalized %>%
  st_drop_geometry() %>%
  # --- PERBAIKAN DI SINI: Gunakan dplyr::select() secara eksplisit ---
  dplyr::select(all_of(vars_for_score)) %>%
  rowSums(na.rm = TRUE)

# Tambahkan skor dan buat klaster. Objek 'data_peta' sekarang menjadi hasil akhir untuk peta.
data_peta <- data_peta_normalized %>%
  mutate(
    Skor_Kerentanan = skor_vector,
    Cluster = case_when(
      Skor_Kerentanan <= 4.80 ~ "Kelompok 1 (Rendah)",
      Skor_Kerentanan > 4.80 & Skor_Kerentanan <= 6.09 ~ "Kelompok 2 (Sedang)",
      Skor_Kerentanan > 6.09 & Skor_Kerentanan <= 7.47 ~ "Kelompok 3 (Tinggi)",
      Skor_Kerentanan > 7.47 ~ "Kelompok 4 (Sangat Tinggi)",
      TRUE ~ "Tidak Terdefinisi"
    )
  )

# Membuat data frame untuk metadata variabel
metadata_variabel <- data.frame(
  Variabel = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
  Deskripsi = c(
    "Kode unik untuk setiap kabupaten/kota.",
    "Persentase penduduk berusia di bawah lima tahun.",
    "Persentase penduduk perempuan.",
    "Persentase penduduk berusia 65 tahun ke atas.",
    "Persentase rumah tangga yang dikepalai oleh perempuan.",
    "Rata-rata jumlah anggota rumah tangga.",
    "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan utama.",
    "Persentase penduduk berusia 15 tahun ke atas dengan tingkat pendidikan rendah.",
    "Persentase perubahan (pertumbuhan) populasi.",
    "Persentase penduduk miskin.",
    "Persentase penduduk yang tidak bisa membaca dan menulis.",
    "Persentase rumah tangga yang tidak pernah mendapatkan pelatihan mitigasi bencana.",
    "Persentase rumah tangga yang tinggal di daerah rawan bencana.",
    "Persentase rumah tangga yang menyewa rumah.",
    "Persentase rumah tangga yang tidak memiliki sistem drainase/pembuangan limbah.",
    "Persentase rumah tangga yang menggunakan air ledeng (pipa).",
    "Jumlah total populasi."
  ))

# =================================================================
##                           USER INTERFACE (UI)                     
## =================================================================

# Custom CSS untuk tema baru
custom_css <- tags$head(
  tags$style(HTML("
    /* Color Palette Variables */
    :root {
      --dark-green: #004030;
      --medium-green: #4A9782;
      --light-beige: #DCD0A8;
      --cream: #FFF9E5;
    }
    
    /* Main body styling */
    .content-wrapper, .right-side {
      background-color: var(--cream) !important;
    }
    
    /* Sidebar styling */
    .skin-blue .main-sidebar {
      background-color: var(--dark-green) !important;
    }
    
    .skin-blue .sidebar-menu > li > a {
      color: var(--cream) !important;
      border-radius: 8px !important;
      margin: 2px 8px !important;
      transition: all 0.3s ease !important;
    }
    
    .skin-blue .sidebar-menu > li:hover > a,
    .skin-blue .sidebar-menu > li.active > a {
      background-color: var(--medium-green) !important;
      color: white !important;
      box-shadow: 0 3px 10px rgba(74, 151, 130, 0.3) !important;
      transform: translateX(5px) !important;
    }
    
    .skin-blue .sidebar-menu > li.header {
      color: var(--light-beige) !important;
      background-color: rgba(74, 151, 130, 0.2) !important;
    }
    
    /* Header styling */
    .skin-blue .main-header .navbar {
      background-color: var(--dark-green) !important;
    }
    
    .skin-blue .main-header .navbar .nav > li > a {
      color: var(--cream) !important;
    }
    
    .skin-blue .main-header .logo {
      background-color: var(--medium-green) !important;
      color: white !important;
      font-weight: bold !important;
      font-size: 24px !important;
    }
    
    /* Box styling */
    .box {
      border-radius: 12px !important;
      box-shadow: 0 4px 15px rgba(0, 64, 48, 0.1) !important;
      border: none !important;
      margin-bottom: 25px !important;
      z-index: 1 !important;
      
    }
    
    .box.box-primary .box-header {
      background-color: var(--medium-green) !important;
      color: white !important;
      border-radius: 12px 12px 0 0 !important;
    }
    
    .box.box-info .box-header {
      background-color: var(--light-beige) !important;
      color: var(--dark-green) !important;
      border-radius: 12px 12px 0 0 !important;
    }
    
    .box.box-success .box-header {
      background-color: var(--medium-green) !important;
      color: white !important;
      border-radius: 12px 12px 0 0 !important;
    }
    
    .box.box-warning .box-header {
      background-color: #E8A317 !important;
      color: white !important;
      border-radius: 12px 12px 0 0 !important;
    }
    
    .box.box-danger .box-header {
      background-color: #DC3545 !important;
      color: white !important;
      border-radius: 12px 12px 0 0 !important;
    }
    
    /* Button styling */
    .btn-primary {
      background-color: var(--medium-green) !important;
      border-color: var(--medium-green) !important;
      border-radius: 8px !important;
      transition: all 0.3s ease !important;
    }
    
    .btn-primary:hover {
      background-color: var(--dark-green) !important;
      border-color: var(--dark-green) !important;
      transform: translateY(-2px) !important;
      box-shadow: 0 5px 15px rgba(0, 64, 48, 0.3) !important;
    }
    
    .btn-success {
      background-color: #28A745 !important;
      border-color: #28A745 !important;
      border-radius: 8px !important;
      transition: all 0.3s ease !important;
    }
    
    .btn-success:hover {
      background-color: #218838 !important;
      transform: translateY(-2px) !important;
      box-shadow: 0 5px 15px rgba(40, 167, 69, 0.3) !important;
    }
    
    /* Input styling */
    .form-control {
      border-radius: 8px !important;
      border: 2px solid var(--light-beige) !important;
      transition: all 0.3s ease !important;
    }
    
    .form-control:focus {
      border-color: var(--medium-green) !important;
      box-shadow: 0 0 0 0.2rem rgba(74, 151, 130, 0.25) !important;
    }
    
    /* DataTable styling */
    .dataTables_wrapper .dataTables_filter input {
      border: 2px solid var(--light-beige) !important;
      border-radius: 8px !important;
    }
    
    .dataTables_wrapper .dataTables_length select {
      border: 2px solid var(--light-beige) !important;
      border-radius: 8px !important;
    }
    
    /* Tab styling */
    .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: var(--medium-green) !important;
    }
    
    .nav-tabs-custom > .nav-tabs > li.active > a {
      background-color: var(--cream) !important;
      color: var(--dark-green) !important;
    }
    
    .nav-tabs-custom > .nav-tabs > li > a {
      color: var(--dark-green) !important;
      border-radius: 8px 8px 0 0 !important;
    }
    
    .nav-tabs-custom > .nav-tabs > li > a:hover {
      background-color: var(--light-beige) !important;
      color: var(--dark-green) !important;
    }
    
    /* Progress bar styling */
    .progress-bar {
      background-color: var(--medium-green) !important;
    }
    
    /* Alert styling */
    .alert-info {
      background-color: rgba(220, 208, 168, 0.2) !important;
      border-color: var(--light-beige) !important;
      color: var(--dark-green) !important;
    }
    
    .alert-success {
      background-color: rgba(74, 151, 130, 0.2) !important;
      border-color: var(--medium-green) !important;
      color: var(--dark-green) !important;
    }
    
    /* Animation effects */
    @keyframes fadeInUp {
      from {
        opacity: 0;
        transform: translate3d(0, 40px, 0);
      }
      to {
        opacity: 1;
        transform: translate3d(0, 0, 0);
      }
    }
    
    .box {
      animation: fadeInUp 0.6s ease-out !important;
    }
    
    /* Hover effects for interactive elements */
    .box:hover {
      transform: translateY(-2px) !important;
      box-shadow: 0 8px 25px rgba(0, 64, 48, 0.15) !important;
      transition: all 0.3s ease !important;
    }
    
    .bootstrap-select .dropdown-menu {
          z-index: 10000 !important;
    }
  "))
)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SIKSI Indonesia"),
  
  # ------ Sidebar Menu ------ #
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Kategori Data", tabName = "kategori", icon = icon("layer-group")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line")),
      menuItem("Peta Klaster", tabName = "peta_klaster", icon = icon("map-marked-alt")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-double")),
      menuItem("Analisis Inferensia", icon = icon("calculator"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_rata2"),
               menuSubItem("Uji Proporsi", tabName = "uji_proporsi"),
               menuSubItem("Uji Varians", tabName = "uji_varians"),
               menuSubItem("Uji ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear", tabName = "regresi", icon = icon("chart-line"))
    )
  ),
  
  # ------ Body Content ------ #
  dashboardBody(
    custom_css,  # Add custom CSS
    tabItems(
      # -- Beranda -- #
      tabItem(tabName = "beranda",
              fluidRow(
                # Kolom Kiri: Informasi Umum & Metodologi
                column(width = 7,
                       box(width = NULL, title = tagList(icon("info-circle"), "Selamat Datang di Dasbor Kerentanan Sosial"), status = "primary", solidHeader = TRUE,
                           h4("Tentang Dasbor Ini"),
                           p("Dasbor ini dirancang untuk melakukan analisis statistik terhadap data kerentanan sosial di 511 kabupaten/kota di Indonesia. Dasbor ini menyediakan fitur untuk melakukan eksplorasi data, analisis klaster spasial, hingga analisis inferensia seperti uji asumsi dan regresi linear berganda."),
                           hr(),
                           h4("Data dan Metodologi"),
                           p(strong("Dataset: "), "511 Kabupaten/Kota di Indonesia"),
                           p(strong("Variabel: "), "Indikator-indikator kerentanan sosial (lihat tabel di samping)"),
                           p(strong("Metode Utama: "), "Analisis Klaster Spasial dengan Matriks Jarak"),
                           p(strong("Jumlah Klaster: "), "4 (Rendah, Sedang, Tinggi, Sangat Tinggi)")
                       ),
                       box(width = NULL, title = tagList(icon("download"), "Unduh Aset"), status = "success", solidHeader = TRUE,
                           p("Silakan unduh dataset dan aset pendukung yang digunakan dalam analisis ini."),
                           # Tombol Download
                           downloadButton("download_sovi", "Data SOVI (.xlsx)", class="btn-success"),
                           downloadButton("download_geojson", "Data Peta (.geojson)", class="btn-success"),
                           downloadButton("download_matriks", "Matriks Jarak (.xlsx)", class="btn-success"),
                           downloadButton("download_paper", "Referensi Jurnal (.pdf)", class="btn-success", icon = icon("file-pdf-o"))
                       )
                ),
                
                # Kolom Kanan: Tabel Metadata Variabel
                column(width = 5,
                       box(width = NULL, title = tagList(icon("table"), "Metadata Variabel"), status = "info", solidHeader = TRUE,
                           p("Tabel ini berisi penjelasan untuk setiap variabel yang digunakan dalam analisis."),
                           DT::dataTableOutput("tabel_metadata")
                       )
                )
              )
      ),
      
      # -- Kategori Data -- #
      tabItem(tabName = "kategori",
              fluidRow(
                box(width = 4, title = "Pengaturan Kategori", status = "primary", solidHeader = TRUE,
                    selectInput("var_kategori", "Pilih Variabel untuk Dikategorikan:",
                                choices = vars_for_kategori_dropdown),
                    p("Setiap variabel akan secara otomatis dibagi menjadi 4 kelompok (Rendah, Sedang, Tinggi, Sangat Tinggi) berdasarkan rentang nilainya.")
                ),
                box(width = 8, title = "Data dengan Kategori Baru", status = "primary", solidHeader = TRUE,
                    DTOutput("tabel_kategori")
                ),
                box(width = 12, title = "Interpretasi & Unduh Laporan", status = "info", solidHeader = TRUE,
                    # Isi interpretasi akan muncul di sini
                    uiOutput("interpretasi_kategori"),
                    
                    hr(), # Garis pemisah
                    
                    # Tombol Download ditambahkan di sini
                    h5("Unduh hasil analisis:"),
                    downloadButton("download_data_kategori", "Unduh Data (.csv)", class = "btn-primary"),
                    downloadButton("download_interpretasi_kategori", "Unduh Interpretasi (.docx)", class = "btn-primary")
                )
              )
      ),
      
      # -- Eksplorasi Data -- #
      tabItem(tabName = "eksplorasi",
              tabsetPanel(
                # --- Tab Statistik Deskriptif --- #
                tabPanel("Statistik Deskriptif",
                         br(),
                         selectInput("var_deskriptif", "Pilih Variabel:", choices = vars_for_eksplorasi),
                         box(width = 12, title = "Ringkasan Statistik", status = "primary", solidHeader = TRUE,
                             verbatimTextOutput("output_deskriptif")
                         ),
                         box(width = 12, title = "Interpretasi & Laporan", status = "info", solidHeader = TRUE,
                             uiOutput("interpretasi_deskriptif"),
                             hr(),
                             downloadButton("download_deskriptif", "Unduh Laporan (.docx)")
                         )
                ),
                
                # --- Tab Visualisasi Grafik --- #
                tabPanel("Visualisasi Grafik",
                         br(),
                         # -- Baris untuk Grafik Individual -- #
                         fluidRow(
                           box(width = 6, title = "Boxplot Sebaran Data", status = "primary", solidHeader = TRUE,
                               selectInput("var_boxplot", "Pilih Variabel:", choices = vars_for_eksplorasi),
                               plotOutput("output_boxplot")
                           ),
                           box(width = 6, title = "Barchart Berdasarkan Indeks", status = "primary", solidHeader = TRUE,
                               selectInput("var_barchart", "Pilih Variabel (Y):", choices = vars_for_eksplorasi),
                               plotOutput("output_barchart")
                           )
                         ),
                         # -- Baris untuk Interpretasi & Laporan Grafik Individual -- #
                         fluidRow(
                           box(width = 6, title = "Interpretasi & Laporan Boxplot", status = "info", solidHeader = TRUE,
                               uiOutput("interpretasi_boxplot"),
                               hr(),
                               downloadButton("download_boxplot", "Unduh Laporan (.docx)")
                           ),
                           box(width = 6, title = "Interpretasi & Laporan Barchart", status = "info", solidHeader = TRUE,
                               uiOutput("interpretasi_barchart"),
                               hr(),
                               downloadButton("download_barchart", "Unduh Laporan (.docx)")
                           )
                         ),
                         # -- Baris untuk Grafik & Laporan Boxplot Gabungan -- #
                         fluidRow(
                           box(width = 12, title = "Boxplot Gabungan Semua Variabel", status = "primary", solidHeader = TRUE,
                               plotOutput("output_boxplot_combined", height = "600px")
                           ),
                           box(width = 12, title = "Interpretasi & Laporan Boxplot Gabungan", status = "info", solidHeader = TRUE,
                               uiOutput("interpretasi_boxplot_combined"),
                               hr(),
                               downloadButton("download_boxplot_combined", "Unduh Laporan (.docx)")
                           )
                         )
                )
              )
      ),
      
      tabItem(tabName = "peta_klaster",
              fluidRow(
                box(width = 12, title = "Peta Klaster Kerentanan Sosial Indonesia", status = "primary", solidHeader = TRUE,
                    leafletOutput("output_peta_klaster", height = "650px")
                ),
                box(width = 12, title = "Interpretasi & Unduh Laporan", status = "info", solidHeader = TRUE,
                    uiOutput("interpretasi_peta_klaster"),
                    hr(),
                    downloadButton("download_peta_klaster", "Unduh Laporan Peta (.docx)")
                )
              )
      ),
      
      # -- Uji Asumsi -- #
      tabItem(tabName = "asumsi",
              tabsetPanel(
                # --- Tab Uji Normalitas --- #
                tabPanel("Uji Normalitas",
                         br(),
                         selectInput("var_normalitas", "Pilih Variabel:", choices = vars_for_assumption),
                         fluidRow(
                           box(width = 6, title = "Q-Q Plot", plotOutput("plot_qq")),
                           box(width = 6, title = "Hasil Uji Shapiro-Wilk", verbatimTextOutput("test_shapiro"))
                         ),
                         box(width = 12, title = "Interpretasi & Laporan", status = "info", solidHeader = TRUE,
                             uiOutput("interpretasi_normalitas"),
                             hr(),
                             downloadButton("download_normalitas", "Unduh Laporan (.docx)")
                         )
                ),
                
                # --- Tab Uji Homogenitas Varians --- #
                tabPanel("Uji Homogenitas Varians",
                         br(),
                         p("Pilih satu variabel. Aplikasi akan menguji apakah varians dari variabel tersebut sama di 4 kelompok (Rendah, Sedang, Tinggi, Sangat Tinggi) yang dibuat secara otomatis."),
                         fluidRow(
                           box(width = 4, title = "Pengaturan Uji",
                               selectInput("var_homogen", "Pilih Variabel:", choices = vars_for_assumption),
                               actionButton("run_homogen_test", "Jalankan Uji")
                           ),
                           box(width = 8, title = "Hasil Uji",
                               verbatimTextOutput("test_homogenitas")
                           )
                         ),
                         box(width = 12, title = "Interpretasi & Laporan", status = "info", solidHeader = TRUE,
                             uiOutput("interpretasi_homogenitas"),
                             hr(),
                             downloadButton("download_homogenitas", "Unduh Laporan (.docx)")
                         )
                )
              )
      ),
      
      # -- Uji Beda Rata-rata -- #
      tabItem(tabName = "uji_rata2",
              h2("Uji Beda Rata-rata (T-Test)"),
              fluidRow(
                box(width = 4, title = "Pengaturan Uji", status = "primary", solidHeader = TRUE,
                    radioButtons("tipe_uji_rata2", "Pilih Tipe Uji:", choices = c("1 Kelompok", "2 Kelompok"), selected = "1 Kelompok"),
                    selectInput("var_uji_rata2", "Pilih Variabel:", choices = vars_for_assumption),
                    # Input dinamis untuk uji 1 kelompok
                    uiOutput("ui_nilai_mu"),
                    actionButton("run_uji_rata2", "Jalankan Uji", icon = icon("play"))
                ),
                box(width = 8, title = "Hasil Uji Statistik", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("hasil_uji_rata2")
                ),
                box(width = 12, title = "Interpretasi Hasil", status = "info", solidHeader = TRUE,
                    uiOutput("interpretasi_uji_rata2")
                )
              )
      ),
      
      # -- Uji Proporsi -- #
      tabItem(tabName = "uji_proporsi",
              h2("Uji Proporsi"),
              fluidRow(
                box(width = 4, title = "Pengaturan Uji", status = "primary", solidHeader = TRUE,
                    p("Catatan: Uji ini mengubah data numerik menjadi biner (di atas/di bawah median) untuk menghitung proporsi."),
                    radioButtons("tipe_uji_proporsi", "Pilih Tipe Uji:", choices = c("1 Kelompok", "2 Kelompok"), selected = "1 Kelompok"),
                    selectInput("var_uji_proporsi", "Pilih Variabel:", choices = vars_for_assumption),
                    uiOutput("ui_nilai_p0"),
                    actionButton("run_uji_proporsi", "Jalankan Uji", icon = icon("play"))
                ),
                box(width = 8, title = "Hasil Uji Statistik", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("hasil_uji_proporsi")
                ),
                box(width = 12, title = "Interpretasi Hasil", status = "info", solidHeader = TRUE,
                    uiOutput("interpretasi_uji_proporsi")
                )
              )
      ),
      
      # -- Uji Varians -- #
      tabItem(tabName = "uji_varians",
              h2("Uji Kesamaan Varians (Ragam)"),
              fluidRow(
                box(width = 4, title = "Pengaturan Uji", status = "primary", solidHeader = TRUE,
                    radioButtons("tipe_uji_varians", "Pilih Tipe Uji:", choices = c("1 Kelompok", "2 Kelompok"), selected = "1 Kelompok"),
                    selectInput("var_uji_varians", "Pilih Variabel:", choices = vars_for_assumption),
                    uiOutput("ui_nilai_sigma2"),
                    actionButton("run_uji_varians", "Jalankan Uji", icon = icon("play"))
                ),
                box(width = 8, title = "Hasil Uji Statistik", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("hasil_uji_varians")
                ),
                box(width = 12, title = "Interpretasi Hasil", status = "info", solidHeader = TRUE,
                    uiOutput("interpretasi_uji_varians")
                )
              )
      ),
      
      # -- Uji ANOVA -- #
      tabItem(tabName = "anova",
              h2("Analisis Ragam (ANOVA)"),
              tabsetPanel(
                # --- ANOVA SATU ARAH --- #
                tabPanel("ANOVA Satu Arah (One-Way)",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE,
                               fluidRow(
                                 column(6, selectInput("var_anova1_y", "1. Pilih Variabel Dependen (Y, Numerik):", choices = vars_for_assumption)),
                                 column(6, selectInput("var_anova1_x", "2. Pilih Variabel untuk Faktor (X, akan dikategorikan):", choices = vars_for_assumption))
                               ),
                               actionButton("run_anova1", "Jalankan Analisis ANOVA Satu Arah", icon = icon("play"))
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "1. Pemeriksaan Asumsi", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                               h4("Asumsi Homogenitas Ragam (Levene's Test)"),
                               verbatimTextOutput("anova1_asumsi_homogen"),
                               h4("Asumsi Normalitas Residual (Shapiro-Wilk Test)"),
                               verbatimTextOutput("anova1_asumsi_normal"),
                               plotOutput("anova1_plot_qq", height = "300px"),
                               uiOutput("anova1_info_transformasi")
                           )
                         ),
                         fluidRow(
                           box(width = 6, title = "2. Hasil Analisis Ragam (ANOVA)", status = "success", solidHeader = TRUE, collapsible = TRUE,
                               verbatimTextOutput("anova1_hasil_uji"),
                               uiOutput("interpretasi_anova1_uji")
                           ),
                           box(width = 6, title = "3. Hasil Uji Lanjut (Tukey HSD)", status = "success", solidHeader = TRUE, collapsible = TRUE,
                               verbatimTextOutput("anova1_hasil_lanjut"),
                               uiOutput("interpretasi_anova1_lanjut")
                           )
                         )
                ),
                
                # --- ANOVA DUA ARAH --- #
                tabPanel("ANOVA Dua Arah (Two-Way)",
                         br(),
                         fluidRow(
                           box(width = 12, title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE,
                               p("Pastikan Anda sudah memahami hubungan antar variabel dari ANOVA Satu Arah sebelum melanjutkan."),
                               fluidRow(
                                 column(4, selectInput("var_anova2_y", "1. Pilih Variabel Dependen (Y, Numerik):", choices = vars_for_assumption)),
                                 column(4, selectInput("var_anova2_x1", "2. Pilih Variabel untuk Faktor 1 (X1):", choices = vars_for_assumption)),
                                 column(4, selectInput("var_anova2_x2", "3. Pilih Variabel untuk Faktor 2 (X2):", choices = vars_for_assumption))
                               ),
                               actionButton("run_anova2", "Jalankan Analisis ANOVA Dua Arah", icon = icon("play"))
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "1. Pemeriksaan Asumsi", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                               h4("Asumsi Homogenitas Ragam (Levene's Test)"),
                               verbatimTextOutput("anova2_asumsi_homogen"),
                               h4("Asumsi Normalitas Residual (Shapiro-Wilk Test)"),
                               verbatimTextOutput("anova2_asumsi_normal")
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "2. Hasil Analisis Ragam (ANOVA)", status = "success", solidHeader = TRUE, collapsible = TRUE,
                               verbatimTextOutput("anova2_hasil_uji"),
                               uiOutput("interpretasi_anova2_uji")
                           )
                         ),
                         fluidRow(
                           box(width = 6, title = "3. Hasil Uji Lanjut (Tukey HSD)", status = "success", solidHeader = TRUE, collapsible = TRUE,
                               verbatimTextOutput("anova2_hasil_lanjut")
                           ),
                           box(width = 6, title = "4. Visualisasi Boxplot", status = "success", solidHeader = TRUE, collapsible = TRUE,
                               plotOutput("anova2_plot_boxplot")
                           )
                         )
                )
              )
      ),
      
      # Analis Regresi
      tabItem(tabName = "regresi",
              fluidRow(
                box(width = 12, title = "Pengaturan Model Regresi", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(6,
                             selectInput("model_regresi", "1. Pilih Model Awal:",
                                         choices = c("Model 1: POVERTY ~ f(Pendidikan, Infrastruktur)",
                                                     "Model 2: POPULATION ~ f(Demografi)",
                                                     "Model 3: POVERTY ~ f(Struktur Keluarga)",
                                                     "Model 4: LOWEDU ~ f(Kondisi Hidup)",
                                                     "Model 5: NOTRAINING ~ f(Kerentanan & Paparan)"))
                      ),
                      column(6,
                             pickerInput(
                               inputId = "additional_vars_x",
                               label = "2. Tambah/Kurangi Variabel Independen (X):",
                               choices = vars_for_assumption,
                               multiple = TRUE,
                               options = list(`actions-box` = TRUE, `live-search` = TRUE)
                             )
                      )
                    ),
                    actionButton("run_regresi", "Jalankan Analisis Regresi", icon = icon("play"), width = "100%", class = "btn-success"),
                    
                    br(), # Tambah sedikit spasi
                    
                    # Tombol Download ditambahkan di sini
                    downloadButton("download_laporan_regresi", "Unduh Laporan Lengkap (.docx)", icon = icon("file-word-o"), width = "100%")
                ),
                
                # --- PERUBAHAN DI SINI ---
                # Box untuk Summary diubah menjadi width = 8
                box(width = 6, title = "Hasil Model Regresi Awal (Summary)", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    verbatimTextOutput("summary_regresi")
                ),
                
                # BOX BARU DITAMBAHKAN untuk Interpretasi Model
                box(width = 6, title = "Interpretasi Model", status = "info", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("interpretasi_regresi_summary")
                ),
                
                # ------------------------
                
                box(width = 12, title = "Pemeriksaan Asumsi Klasik Model Awal", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("summary_assumptions")
                ),
                
                # BOX untuk Perbaikan Model
                uiOutput("ui_perbaikan_asumsi"),
                
                # BOX untuk Hasil Setelah Perbaikan
                uiOutput("ui_hasil_perbaikan")
              )
      )
    )
  )
)

# =================================================================
##                           SERVER LOGIC                            
## =================================================================

server <- function(input, output, session) {
  
  # Tampilkan tabel metadata variabel
  output$tabel_metadata <- DT::renderDataTable({
    DT::datatable(
      metadata_variabel,
      options = list(
        pageLength = 10,
        dom = 'ftp', # f: filtering, t: table, p: pagination
        language = list(search = "Cari:")
      ),
      rownames = FALSE,
      caption = "Daftar Variabel dan Deskripsinya"
    )
  })
  
  # Logika untuk tombol download
  output$download_sovi <- downloadHandler(
    filename = function() {
      "sovi_data.xlsx"
    },
    content = function(file) {
      file.copy("sovi_data.xlsx", file)
    }
  )
  
  output$download_geojson <- downloadHandler(
    filename = function() {
      "indonesia511.geojson"
    },
    content = function(file) {
      file.copy("indonesia511.geojson", file)
    }
  )
  
  output$download_matriks <- downloadHandler(
    filename = function() {
      "matrik_penimbang_jarak.xlsx"
    },
    content = function(file) {
      # Pastikan nama file di sini sama persis dengan nama file Anda
      file.copy("matrik penimbang jarak.xlsx", file)  
    }
  )
  
  output$download_paper <- downloadHandler(
    filename = function() {
      # Nama file yang akan diunduh oleh pengguna
      "Referensi Jurnal Dashboard.pdf"
    },
    content = function(file) {
      # Salin file dari folder lokal Anda ke output unduhan
      file.copy("referensi jurnal dashboard.pdf", file)
    }
  )
  
  # ------ Kategori Data ------ #
  data_kategorik <- reactive({
    req(input$var_kategori)
    var <- input$var_kategori
    n <- 4 # Jumlah kategori sudah benar 4
    
    # ---- PERUBAHAN DI SINI ----
    # Buat label kategori secara spesifik
    labels <- c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi")
    
    # Salin data asli dan tambahkan kolom kategori
    df <- sovi_data
    df[[paste0(var, "_CAT")]] <- cut(df[[var]],
                                     breaks = n,
                                     labels = labels,
                                     include.lowest = TRUE,
                                     ordered_result = TRUE)
    df
  })
  
  # Bagian ini tidak perlu diubah, biarkan seperti semula
  observe({
    req(data_kategorik()) # Pastikan data_kategorik tidak kosong
    kategorik_cols <- names(data_kategorik())[grepl("_CAT", names(data_kategorik()))]
    updateSelectInput(session, "var_homogen_cat", choices = kategorik_cols)
    
    model_choice <- input$model_regresi
    
    # Tentukan variabel default berdasarkan pilihan model
    default_vars <- switch(model_choice,
                           "Model 1: POVERTY ~ f(Pendidikan, Infrastruktur)" = c("NOELECTRIC", "ILLITERATE", "DPRONE"),
                           "Model 2: POPULATION ~ f(Demografi)" = c("FAMILYSIZE", "ELDERLY"),
                           "Model 3: POVERTY ~ f(Struktur Keluarga)" = c("ELDERLY", "FAMILYSIZE"),
                           "Model 4: LOWEDU ~ f(Kondisi Hidup)" = c("NOELECTRIC", "RENTED", "POVERTY", "NOSEWER"),
                           "Model 5: NOTRAINING ~ f(Kerentanan & Paparan)" = c("POVERTY", "LOWEDU", "DPRONE", "ELDERLY")
    )
    
    # Update dropdown pickerInput dengan pilihan default
    updatePickerInput(session, "additional_vars_x", selected = default_vars)
  })
  
  output$tabel_kategori <- renderDT({
    datatable(data_kategorik(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$interpretasi_kategori <- renderUI({
    req(input$var_kategori)
    var <- input$var_kategori
    n <- 4 # Jumlah kategori sudah benar 4
    
    # Dapatkan rentang nilai untuk setiap kategori
    df <- data_kategorik()
    col_cat <- paste0(var, "_CAT")
    
    summary_by_cat <- df %>%
      group_by(.data[[col_cat]]) %>%
      summarise(Min = min(.data[[var]], na.rm = TRUE),
                Max = max(.data[[var]], na.rm = TRUE),
                .groups = 'drop') %>%
      # Pastikan tidak ada NA di hasil summary
      filter(!is.na(.data[[col_cat]]))
    
    interpretasi_list <- lapply(1:nrow(summary_by_cat), function(i) {
      p(strong(summary_by_cat[[col_cat]][i], ":"),
        paste0("Mencakup nilai '", var, "' dari ",
               round(summary_by_cat$Min[i], 2), " hingga ", round(summary_by_cat$Max[i], 2), "."))
    })
    
    tagList(
      h5(paste("Variabel '", var, "' telah dibagi menjadi", n, "kelompok kategori:")),
      interpretasi_list
    )
  })
  
  # --- Logika untuk Download Hasil Kategori --- #
  
  # 1. Download Data Kategori (.csv)
  output$download_data_kategori <- downloadHandler(
    filename = function() {
      paste0("data_kategori_", input$var_kategori, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Menggunakan data_kategorik() yang sudah reaktif
      write.csv(data_kategorik(), file, row.names = FALSE)
    }
  )
  
  output$download_interpretasi_kategori <- downloadHandler(
    filename = function() {
      paste0("interpretasi_kategori_", input$var_kategori, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      withProgress(message = 'Membuat laporan Word...', value = 0, {
        
        # Langkah 1: Siapkan parameter
        var <- input$var_kategori
        df <- data_kategorik()
        col_cat <- paste0(var, "_CAT")
        
        # --- PERUBAHAN DI SINI ---
        # Tambahkan `Jumlah_Data = n()` untuk menghitung jumlah data per kelompok
        summary_by_cat <- df %>%
          group_by(.data[[col_cat]]) %>%
          summarise(
            Jumlah_Data = n(), # <--- BARIS INI DITAMBAHKAN
            Min = min(.data[[var]], na.rm = TRUE),
            Max = max(.data[[var]], na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          filter(!is.na(.data[[col_cat]]))
        
        params_to_pass <- list(
          nama_variabel = var,
          jumlah_kategori = 4,
          summary_df = summary_by_cat
        )
        
        incProgress(0.5)
        
        # Langkah 2: Render file .Rmd 
        # Pastikan path file sudah benar menuju folder 'template'
        rmarkdown::render(
          "template/interpretasi_kategori.Rmd", 
          output_file = file,
          params = params_to_pass,
          envir = new.env(parent = globalenv())
        )
        
        incProgress(1)
      })
    }
  )
  
  # ------ Eksplorasi Data ------ #
  
  # --- BAGIAN 1: Objek Reaktif untuk Setiap Output ---
  
  # -- A. Reaktif untuk Statistik Deskriptif --
  deskriptif_summary_reactive <- reactive({
    req(input$var_deskriptif)
    x <- sovi_data[[input$var_deskriptif]]
    validate(need(is.numeric(x), "Variabel bukan numerik."))
    
    # Meng-capture output teks ringkasan
    capture.output({
      cat("Ringkasan Umum (5 Angka + Mean):\n")
      print(summary(x))
      cat("\nUkuran Sebaran:\n")
      cat("  Varians          :", var(x, na.rm = TRUE), "\n")
      cat("  Standar Deviasi  :", sd(x, na.rm = TRUE), "\n")
      cat("\nUkuran Bentuk Distribusi:\n")
      cat("  Skewness         :", moments::skewness(x, na.rm = TRUE), "\n")
      cat("  Kurtosis         :", moments::kurtosis(x, na.rm = TRUE), "\n")
    })
  })
  
  deskriptif_interp_reactive <- reactive({
    req(input$var_deskriptif)
    x <- sovi_data[[input$var_deskriptif]]
    # Pengaman: Pastikan kolom benar-benar numerik
    validate(
      need(is.numeric(x), "") # Jangan tampilkan pesan error di sini
    )
    
    sk <- moments::skewness(x, na.rm = TRUE)
    ku <- moments::kurtosis(x, na.rm = TRUE)
    
    # Interpretasi Skewness
    if (sk > 0.5) {
      interp_sk <- "<b>Positif (Menceng ke Kanan)</b>, artinya ekor distribusi lebih panjang ke arah kanan dan sebagian besar data terkonsentrasi di nilai rendah."
    } else if (sk < -0.5) {
      interp_sk <- "<b>Negatif (Menceng ke Kiri)</b>, artinya ekor distribusi lebih panjang ke arah kiri dan sebagian besar data terkonsentrasi di nilai tinggi."
    } else {
      interp_sk <- "<b>Simetris</b>, artinya sebaran data relatif seimbang di kedua sisi rata-rata."
    }
    
    # Interpretasi Kurtosis
    if (ku > 3) {
      interp_ku <- "<b>Leptokurtik (Runcing)</b>, artinya distribusi memiliki puncak yang lebih tajam dan ekor yang lebih tebal dibandingkan distribusi normal. Ini mengindikasikan adanya potensi outlier yang lebih banyak."
    } else if (ku < 3) {
      interp_ku <- "<b>Platykurtik (Datar)</b>, artinya distribusi memiliki puncak yang lebih datar dan ekor yang lebih tipis dibandingkan distribusi normal."
    } else {
      interp_ku <- "<b>Mesokurtik (Normal)</b>, artinya tingkat keruncingan distribusi mendekati normal."
    }
    
    HTML(paste0(
      "<ul>",
      "<li><b>Varians & Standar Deviasi:</b> Mengukur seberapa jauh data tersebar dari nilai rata-ratanya. Semakin besar nilainya, semakin beragam datanya.</li>",
      "<li><b>Skewness (Kemencengan):</b> Mengukur ketidaksimetrisan distribusi data. Untuk variabel ini, distribusinya ", interp_sk, "</li>",
      "<li><b>Kurtosis (Keruncingan):</b> Mengukur 'keruncingan' puncak distribusi data. Untuk variabel ini, distribusinya ", interp_ku, "</li>",
      "</ul>"
    ))
  })
  
  # -- B. Reaktif untuk Boxplot Individual --
  plot_boxplot_reactive <- reactive({
    req(input$var_boxplot)
    ggplot(sovi_data, aes_string(y = input$var_boxplot)) +
      geom_boxplot(fill = "#4A9782", color = "#004030", alpha = 0.7) +
      labs(title = paste("Boxplot Sebaran Variabel", input$var_boxplot), y = input$var_boxplot) +
      theme_minimal()
  })
  interp_boxplot_reactive <- reactive({
    req(input$var_boxplot)
    paste0("Boxplot ini menunjukkan sebaran data untuk variabel '", input$var_boxplot, 
           "'. Garis tengah di dalam kotak adalah median (nilai tengah). Kotak menunjukkan rentang interkuartil (IQR), di mana 50% data berada. Garis (whiskers) menunjukkan rentang data di luar IQR, dan titik-titik di luar garis adalah potensi outlier (nilai ekstrem).")
  })
  
  # -- C. Reaktif untuk Barchart Individual --
  plot_barchart_reactive <- reactive({
    req(input$var_barchart, sovi_data$NO)
    ggplot(sovi_data, aes_string(x = "NO", y = input$var_barchart)) +
      geom_bar(stat = "identity", fill = "#004030") +
      labs(title = paste("Barchart", input$var_barchart, "Berdasarkan Indeks"), x = "Indeks Wilayah (NO)", y = input$var_barchart) +
      theme_minimal()
  })
  interp_barchart_reactive <- reactive({
    req(input$var_barchart)
    paste0("Grafik batang ini menampilkan nilai dari variabel '", input$var_barchart, 
           "' untuk setiap wilayah berdasarkan indeks uniknya ('NO'). Grafik ini berguna untuk melihat variasi nilai antar wilayah secara individual dan mengidentifikasi wilayah dengan nilai tertinggi atau terendah.")
  })
  
  # -- D. Reaktif untuk Boxplot Gabungan --
  plot_boxplot_combined_reactive <- reactive({
    data_to_plot <- sovi_data %>% dplyr::select(all_of(vars_for_eksplorasi))
    data_long <- data_to_plot %>% tidyr::pivot_longer(cols = everything(), names_to = "Variabel", values_to = "Nilai")
    
    ggplot(data_long, aes(x = Variabel, y = Nilai, fill = Variabel)) +
      geom_boxplot(show.legend = FALSE) +
      facet_wrap(~ Variabel, scales = "free_y") + 
      labs(title = "Perbandingan Sebaran Semua Variabel", y = "Nilai", x = "") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  interp_boxplot_combined_reactive <- reactive({
    "Grafik ini menampilkan perbandingan sebaran dari semua variabel secara berdampingan. Setiap kotak mewakili satu variabel. Penggunaan skala Y yang bebas ('free_y') memungkinkan setiap variabel ditampilkan dalam rentang nilainya sendiri, sehingga perbandingan bentuk distribusi, median, dan adanya outlier antar variabel menjadi lebih mudah dilakukan, meskipun skala aslinya berbeda-jauh."
  })
  
  # --- BAGIAN 2: Output untuk Menampilkan di UI ---
  # Sekarang, output hanya perlu memanggil objek reaktif yang sesuai
  
  # -- Statistik Deskriptif --
  output$output_deskriptif <- renderPrint({
    cat(deskriptif_summary_reactive(), sep = '\n')
  })
  output$interpretasi_deskriptif <- renderUI({
    HTML(deskriptif_interp_reactive())
  })
  
  # -- Boxplot Individual --
  output$output_boxplot <- renderPlot({ plot_boxplot_reactive() })
  output$interpretasi_boxplot <- renderUI({ p(interp_boxplot_reactive()) })
  
  # -- Barchart Individual --
  output$output_barchart <- renderPlot({ plot_barchart_reactive() })
  output$interpretasi_barchart <- renderUI({ p(interp_barchart_reactive()) })
  
  # -- Boxplot Gabungan --
  output$output_boxplot_combined <- renderPlot({ plot_boxplot_combined_reactive() })
  output$interpretasi_boxplot_combined <- renderUI({ p(interp_boxplot_combined_reactive()) })
  
  # GANTI SEMUA DOWNLOAD HANDLER EKSPLORASI DATA DI SERVER ANDA DENGAN INI
  
  # --- BAGIAN 3: Output untuk Download Handler (REVISI TOTAL) ---
  
  output$download_deskriptif <- downloadHandler(
    filename = function() { paste0("laporan_deskriptif_", input$var_deskriptif, ".docx") },
    content = function(file) {
      # Handler ini sudah benar dan tidak perlu diubah
      params <- list(
        nama_variabel = input$var_deskriptif,
        summary_text = paste(deskriptif_summary_reactive(), collapse = "\n"),
        interpretasi_text = deskriptif_interp_reactive()
      )
      rmarkdown::render("template/laporan_deskriptif.Rmd", output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_boxplot <- downloadHandler(
    filename = function() { paste0("laporan_boxplot_", input$var_boxplot, ".docx") },
    content = function(file) {
      withProgress(message = 'Membuat laporan...', value = 0, {
        # Langkah 1: Buat path file sementara untuk menyimpan plot
        plot_path <- tempfile(fileext = ".png")
        
        # Langkah 2: Simpan objek plot reaktif ke file sementara
        ggsave(plot_path, plot = plot_boxplot_reactive(), width = 7, height = 5, dpi = 300)
        
        incProgress(0.5)
        
        # Langkah 3: Siapkan parameter, kirim LOKASI FILE PLOT, bukan objeknya
        params <- list(
          nama_variabel = input$var_boxplot,
          plot_file_path = plot_path, # <--- PERUBAHAN KUNCI
          interpretasi_text = interp_boxplot_reactive()
        )
        
        # Langkah 4: Render R Markdown
        rmarkdown::render("template/laporan_boxplot.Rmd", output_file = file, params = params, envir = new.env(parent = globalenv()))
        incProgress(1)
      })
    }
  )
  
  output$download_barchart <- downloadHandler(
    filename = function() { paste0("laporan_barchart_", input$var_barchart, ".docx") },
    content = function(file) {
      withProgress(message = 'Membuat laporan...', value = 0, {
        plot_path <- tempfile(fileext = ".png")
        ggsave(plot_path, plot = plot_barchart_reactive(), width = 7, height = 5, dpi = 300)
        
        incProgress(0.5)
        
        params <- list(
          nama_variabel = input$var_barchart,
          plot_file_path = plot_path, # <--- PERUBAHAN KUNCI
          interpretasi_text = interp_barchart_reactive()
        )
        
        rmarkdown::render("template/laporan_barchart.Rmd", output_file = file, params = params, envir = new.env(parent = globalenv()))
        incProgress(1)
      })
    }
  )
  
  output$download_boxplot_combined <- downloadHandler(
    filename = function() { "laporan_boxplot_gabungan.docx" },
    content = function(file) {
      withProgress(message = 'Membuat laporan...', value = 0, {
        plot_path <- tempfile(fileext = ".png")
        ggsave(plot_path, plot = plot_boxplot_combined_reactive(), width = 8, height = 8, dpi = 300)
        
        incProgress(0.5)
        
        params <- list(
          plot_file_path = plot_path, # <--- PERUBAHAN KUNCI
          interpretasi_text = interp_boxplot_combined_reactive()
        )
        
        rmarkdown::render("template/laporan_boxplot_gabungan.Rmd", output_file = file, params = params, envir = new.env(parent = globalenv()))
        incProgress(1)
      })
    }
  )
  
  # --- Logika Halaman Peta Klaster (REVISI DENGAN FITUR DOWNLOAD) --- #
  
  # 1. Buat objek Peta menjadi reaktif
  peta_klaster_reactive <- reactive({
    # Buat palet warna untuk 4 klaster menggunakan tema baru
    pal <- colorFactor(
      palette = c("#4A9782", "#DCD0A8", "#E8A317", "#DC3545"),
      domain = data_peta$Cluster
    )
    
    # Buat label untuk popup
    popup_labels <- paste(
      "<strong>Provinsi:</strong>", data_peta$nmprov, "<br>",
      "<strong>Kab/Kota:</strong>", data_peta$nmkab, "<br>",
      "<strong>Skor Kerentanan:</strong>", round(data_peta$Skor_Kerentanan, 2), "<br>",
      "<strong>Tingkat Kerentanan:</strong>", data_peta$Cluster
    ) %>% lapply(htmltools::HTML)
    
    # Render peta leaflet
    leaflet(data_peta) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Peta Terang") %>%
      addPolygons(
        fillColor = ~pal(Cluster), weight = 1, opacity = 1, color = "white",
        dashArray = "3", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = popup_labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~Cluster, opacity = 0.8, title = "Tingkat Kerentanan", position = "bottomright")
  })
  
  # 2. Buat objek Interpretasi menjadi reaktif
  interpretasi_peta_klaster_reactive <- reactive({
    HTML(paste0("
        <p>Peta ini memvisualisasikan hasil pengelompokan 511 kabupaten/kota ke dalam 4 klaster berdasarkan tingkat kerentanan sosial. Skor kerentanan dihitung dengan menggabungkan seluruh variabel sosial-ekonomi yang tersedia.</p>
        
        <h4>Deskripsi Klaster:</h4>
        <ul>
            <li><span style='color:#4A9782;'><b>Kelompok 1 (Rendah):</b></span> Wilayah dengan skor kerentanan <strong>kurang dari atau sama dengan 4.80</strong>. Wilayah ini secara umum memiliki kondisi sosial-ekonomi yang paling baik. </li>
            <li><span style='color:#DCD0A8;'><b>Kelompok 2 (Sedang):</b></span> Wilayah dengan skor kerentanan <strong>di atas 4.80 hingga 6.09</strong>. </li>
            <li><span style='color:#E8A317;'><b>Kelompok 3 (Tinggi):</b></span> Wilayah dengan skor kerentanan <strong>di atas 6.09 hingga 7.47</strong>. Wilayah ini memiliki tantangan sosial-ekonomi yang cukup signifikan dan perlu perhatian. </li>
            <li><span style='color:#DC3545;'><b>Kelompok 4 (Sangat Tinggi):</b></span> Wilayah dengan skor kerentanan <strong>di atas 7.47</strong>. Ini adalah wilayah paling rentan yang menjadi prioritas utama untuk intervensi kebijakan. </li>
        </ul>
        <p><i>Arahkan kursor atau klik pada salah satu wilayah di peta untuk melihat detail skor dan klasifikasi klaster untuk wilayah tersebut.</i></p>
    "))
  })
  
  # 3. Tampilkan Peta dan Interpretasi di UI
  output$output_peta_klaster <- renderLeaflet({
    peta_klaster_reactive()
  })
  
  output$interpretasi_peta_klaster <- renderUI({
    interpretasi_peta_klaster_reactive()
  })
  
  # 4. Tambahkan Download Handler
  output$download_peta_klaster <- downloadHandler(
    filename = function() {
      paste0("laporan_peta_klaster_", Sys.Date(), ".docx")
    },
    content = function(file) {
      withProgress(message = 'Membuat laporan peta...', value = 0, {
        
        # Langkah 1: Simpan widget leaflet ke file HTML sementara
        incProgress(0.2, detail = "Menyimpan peta...")
        map_html_path <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(peta_klaster_reactive(), map_html_path, selfcontained = FALSE)
        
        # Langkah 2: Ambil "screenshot" dari file HTML dan simpan sebagai PNG
        incProgress(0.5, detail = "Mengambil gambar peta...")
        map_image_path <- tempfile(fileext = ".png")
        webshot2::webshot(
          url = map_html_path,
          file = map_image_path,
          delay = 2 # Beri waktu 2 detik agar peta sempat termuat
        )
        
        # Langkah 3: Siapkan parameter (sekarang hanya path gambar)
        params_to_pass <- list(
          peta_file_path = map_image_path
        )
        
        # Langkah 4: Render laporan
        incProgress(0.8, detail = "Menyusun dokumen...")
        rmarkdown::render(
          "template/laporan_peta_klaster.Rmd", 
          output_file = file,
          params = params_to_pass,
          envir = new.env(parent = globalenv())
        )
        incProgress(1)
      })
    }
  )
  
  # ------ Uji Asumsi ------ #
  
  # ------ Uji Asumsi (REVISI FINAL DENGAN POLA KONSISTEN) ------ #
  
  # --- BAGIAN 1: UJI NORMALITAS ---
  
  # --- BAGIAN 1: UJI NORMALITAS ---
  
  ## -- A. Komponen Reaktif untuk Normalitas --
  normalitas_res_reactive <- reactive({
    req(input$var_normalitas)
    shapiro.test(sovi_data[[input$var_normalitas]])
  })
  
  plot_qq_reactive <- reactive({
    req(input$var_normalitas)
    df_res <- data.frame(Nilai = sovi_data[[input$var_normalitas]])
    
    ggplot(df_res, aes(sample = Nilai)) +
      stat_qq(color = "#004030", alpha = 0.7) +
      stat_qq_line(color = "#4A9782", linetype = "dashed", linewidth = 1) +
      labs(title = paste("Normal Q-Q Plot untuk", input$var_normalitas), 
           x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal(base_size = 14)
  })
  
  interpretasi_normalitas_reactive <- reactive({
    # Pastikan reactive dependen sudah siap
    req(normalitas_res_reactive())
    res <- normalitas_res_reactive()
    alpha <- 0.05
    # Menggunakan as.character() untuk memastikan outputnya adalah teks biasa
    if (res$p.value < alpha) {
      as.character(HTML(paste0("<p><b>Interpretasi:</b></p><ul><li><b>Hipotesis Nol (H0):</b> Data berdistribusi normal.</li><li><b>P-value:</b> ", round(res$p.value, 5), "</li><li><b>Keputusan:</b> Karena p-value lebih kecil dari alpha, maka kita <b>menolak H0</b>.</li><li><b>Kesimpulan:</b> Terdapat cukup bukti bahwa data <b>tidak berdistribusi normal</b>.</li></ul>")))
    } else {
      as.character(HTML(paste0("<p><b>Interpretasi:</b></p><ul><li><b>Hipotesis Nol (H0):</b> Data berdistribusi normal.</li><li><b>P-value:</b> ", round(res$p.value, 5), "</li><li><b>Keputusan:</b> Karena p-value lebih besar dari alpha, maka kita <b>gagal menolak H0</b>.</li><li><b>Kesimpulan:</b> Data dapat dianggap <b>berdistribusi normal</b>.</li></ul>")))
    }
  })
  
  # -- B. Output Tampilan UI untuk Normalitas --
  output$plot_qq <- renderPlot({ plot_qq_reactive() })
  output$test_shapiro <- renderPrint({ normalitas_res_reactive() })
  output$interpretasi_normalitas <- renderUI({ HTML(interpretasi_normalitas_reactive()) })
  
  # -- C. Download Handler untuk Normalitas --
  output$download_normalitas <- downloadHandler(
    filename = function() {
      paste0("laporan_normalitas_", input$var_normalitas, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      withProgress(message = 'Membuat laporan...', value = 0, {
        
        # Langkah 3: Siapkan parameter, panggil objek reaktif lainnya
        params_to_pass <- list(
          nama_variabel = input$var_normalitas,
          hasil_shapiro_text = paste(capture.output(normalitas_res_reactive()), collapse = "\n"),
          interpretasi_text = interpretasi_normalitas_reactive()
        )
        
        # Langkah 4: Render laporan R Markdown
        rmarkdown::render(
          "template/laporan_normalitas.Rmd",
          output_file = file,
          params = params_to_pass,
          envir = new.env(parent = globalenv())
        )
        incProgress(1)
      })
    }
  )
  
  # --- BAGIAN 2: UJI HOMOGENITAS ---
  
  # -- A. Komponen Reaktif untuk Homogenitas --
  homogen_results_reactive <- eventReactive(input$run_homogen_test, {
    req(input$var_homogen)
    var_cont <- input$var_homogen
    
    df_homogen <- data.frame(kontinyu = sovi_data[[var_cont]])
    df_homogen$kategorik <- cut(df_homogen$kontinyu, breaks = 4,
                                labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                                include.lowest = TRUE)
    
    formula_uji <- as.formula("kontinyu ~ kategorik")
    list(
      bartlett = bartlett.test(formula_uji, data = df_homogen),
      levene = leveneTest(formula_uji, data = df_homogen),
      variable_name = var_cont
    )
  })
  
  interpretasi_homogenitas_reactive <- reactive({
    req(homogen_results_reactive())
    res <- homogen_results_reactive()
    alpha <- 0.05
    if (res$bartlett$p.value < alpha) {
      interp_bartlett <- "<b>menolak H0</b>, artinya varians <b>tidak homogen</b>."
    } else {
      interp_bartlett <- "<b>gagal menolak H0</b>, artinya varians dapat dianggap <b>homogen</b>."
    }
    if (res$levene$`Pr(>F)`[1] < alpha) {
      interp_levene <- "<b>menolak H0</b>, artinya varians <b>tidak homogen</b>."
    } else {
      interp_levene <- "<b>gagal menolak H0</b>, artinya varians dapat dianggap <b>homogen</b>."
    }
    HTML(paste0("<h5>Interpretasi Uji Homogenitas untuk Variabel '", res$variable_name, "' (Alpha = 0.05)</h5>",
                "<p><b>Hipotesis Nol (H0)</b> untuk kedua uji adalah: Varians di setiap kelompok (Rendah, Sedang, Tinggi, Sangat Tinggi) adalah sama (homogen).</p>",
                "<ul><li><b>Uji Bartlett:</b> P-value adalah ", round(res$bartlett$p.value, 5), ". Keputusannya adalah ", interp_bartlett, "</li>",
                "<li><b>Uji Levene:</b> P-value adalah ", round(res$levene$`Pr(>F)`[1], 5), ". Keputusannya adalah ", interp_levene, "</li></ul>",
                "<p><i>Catatan: Uji Levene lebih andal jika data tidak berdistribusi normal.</i></p>"
    ))
  })
  
  # -- B. Output Tampilan UI untuk Homogenitas --
  output$test_homogenitas <- renderPrint({
    res <- homogen_results_reactive()
    cat("Variabel yang diuji:", res$variable_name, "\n\n")
    cat("--- Uji Bartlett ---\n")
    print(res$bartlett)
    cat("\n--- Uji Levene ---\n")
    print(res$levene)
  })
  output$interpretasi_homogenitas <- renderUI({
    interpretasi_homogenitas_reactive()
  })
  
  # -- C. Download Handler untuk Homogenitas --
  output$download_homogenitas <- downloadHandler(
    filename = function() {
      paste0("laporan_homogenitas_", input$var_homogen, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(homogen_results_reactive())
      
      withProgress(message = 'Membuat laporan...', value = 0, {
        res <- homogen_results_reactive()
        
        hasil_uji_text <- capture.output({
          cat("--- Uji Bartlett ---\n"); print(res$bartlett)
          cat("\n--- Uji Levene ---\n"); print(res$levene)
        })
        
        interpretasi_text_html <- as.character(interpretasi_homogenitas_reactive())
        
        incProgress(0.5, detail = "Menyusun dokumen...")
        
        rmarkdown::render(
          "template/laporan_homogenitas.Rmd",
          output_file = file,
          params = list(
            nama_variabel = input$var_homogen,
            hasil_uji_text = paste(hasil_uji_text, collapse = "\n"),
            interpretasi_text = interpretasi_text_html
          ),
          envir = new.env(parent = globalenv())
        )
        incProgress(1)
      })
    }
  )
  
  # --- UI Dinamis untuk Input Nilai --- #
  output$ui_nilai_mu <- renderUI({
    if (input$tipe_uji_rata2 == "1 Kelompok") {
      numericInput("nilai_mu", "Nilai Rata-rata Hipotesis (μ₀):", value = 0, step = 0.1)
    }
  })
  
  output$ui_nilai_p0 <- renderUI({
    if (input$tipe_uji_proporsi == "1 Kelompok") {
      numericInput("nilai_p0", "Nilai Proporsi Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)
    }
  })
  
  output$ui_nilai_sigma2 <- renderUI({
    if (input$tipe_uji_varians == "1 Kelompok") {
      numericInput("nilai_sigma2", "Nilai Varians Hipotesis (σ₀²):", value = 1, min = 0, step = 0.1)
    }
  })
  
  # --- Logika Uji Beda Rata-rata --- #
  hasil_rata2 <- eventReactive(input$run_uji_rata2, {
    req(input$var_uji_rata2)
    data_var <- na.omit(sovi_data[[input$var_uji_rata2]])
    
    if (input$tipe_uji_rata2 == "1 Kelompok") {
      req(input$nilai_mu)
      sampel <- sample(data_var, 100)
      t.test(sampel, mu = input$nilai_mu, alternative = "two.sided")
    } else {
      n_total <- length(data_var)
      idx1 <- sample(1:n_total, 50)
      idx2 <- sample(setdiff(1:n_total, idx1), 50)
      
      grup1 <- data_var[idx1]
      grup2 <- data_var[idx2]
      t.test(grup1, grup2, alternative = "two.sided")
    }
  })
  
  output$hasil_uji_rata2 <- renderPrint({ hasil_rata2() })
  output$interpretasi_uji_rata2 <- renderUI({
    res <- hasil_rata2()
    alpha <- 0.05
    keputusan <- ifelse(res$p.value < alpha, "Menolak H₀", "Gagal Menolak H₀")
    
    if (input$tipe_uji_rata2 == "1 Kelompok") {
      kesimpulan <- ifelse(res$p.value < alpha,
                           "Terdapat cukup bukti untuk menyatakan rata-rata sampel secara signifikan berbeda dari nilai hipotesis.",
                           "Tidak terdapat cukup bukti untuk menyatakan rata-rata sampel berbeda dari nilai hipotesis.")
      HTML(paste0("<b>Hipotesis:</b> H₀: μ = ", input$nilai_mu, " vs H₁: μ ≠ ", input$nilai_mu, "<br>",
                  "<b>P-value:</b> ", round(res$p.value, 5), "<br>",
                  "<b>Keputusan (α=0.05):</b> ", keputusan, "<br>",
                  "<b>Kesimpulan:</b> ", kesimpulan))
    } else {
      kesimpulan <- ifelse(res$p.value < alpha,
                           "Terdapat perbedaan rata-rata yang signifikan antara kedua kelompok.",
                           "Tidak terdapat perbedaan rata-rata yang signifikan antara kedua kelompok.")
      HTML(paste0("<b>Hipotesis:</b> H₀: μ₁ = μ₂ vs H₁: μ₁ ≠ μ₂<br>",
                  "<b>P-value:</b> ", round(res$p.value, 5), "<br>",
                  "<b>Keputusan (α=0.05):</b> ", keputusan, "<br>",
                  "<b>Kesimpulan:</b> ", kesimpulan))
    }
  })
  
  # --- Logika Uji Proporsi --- #
  hasil_proporsi <- eventReactive(input$run_uji_proporsi, {
    req(input$var_uji_proporsi)
    data_var <- na.omit(sovi_data[[input$var_uji_proporsi]])
    median_val <- median(data_var)
    
    if (input$tipe_uji_proporsi == "1 Kelompok") {
      req(input$nilai_p0)
      sampel <- sample(data_var, 100)
      sukses <- sum(sampel > median_val)
      prop.test(sukses, n = 100, p = input$nilai_p0, alternative = "less")
    } else {
      n_total <- length(data_var)
      idx1 <- sample(1:n_total, 50)
      idx2 <- sample(setdiff(1:n_total, idx1), 50)
      
      grup1 <- data_var[idx1]
      grup2 <- data_var[idx2]
      
      sukses1 <- sum(grup1 > median_val)
      sukses2 <- sum(grup2 > median_val)
      
      prop.test(c(sukses1, sukses2), n = c(50, 50), alternative = "two.sided")
    }
  })
  
  output$hasil_uji_proporsi <- renderPrint({ hasil_proporsi() })
  output$interpretasi_uji_proporsi <- renderUI({
    res <- hasil_proporsi()
    alpha <- 0.05
    keputusan <- ifelse(res$p.value < alpha, "Menolak H₀", "Gagal Menolak H₀")
    
    if (input$tipe_uji_proporsi == "1 Kelompok") {
      kesimpulan <- ifelse(res$p.value < alpha,
                           "Terdapat cukup bukti untuk menyatakan proporsi sampel secara signifikan lebih kecil dari nilai hipotesis.",
                           "Tidak terdapat cukup bukti untuk menyatakan proporsi sampel lebih kecil dari nilai hipotesis.")
      HTML(paste0("<b>Hipotesis:</b> H₀: p ≥ ", input$nilai_p0, " vs H₁: p < ", input$nilai_p0, "<br>",
                  "<b>P-value:</b> ", round(res$p.value, 5), "<br>",
                  "<b>Keputusan (α=0.05):</b> ", keputusan, "<br>",
                  "<b>Kesimpulan:</b> ", kesimpulan))
    } else {
      kesimpulan <- ifelse(res$p.value < alpha,
                           "Terdapat perbedaan proporsi yang signifikan antara kedua kelompok.",
                           "Tidak terdapat perbedaan proporsi yang signifikan antara kedua kelompok.")
      HTML(paste0("<b>Hipotesis:</b> H₀: p₁ = p₂ vs H₁: p₁ ≠ p₂<br>",
                  "<b>P-value:</b> ", round(res$p.value, 5), "<br>",
                  "<b>Keputusan (α=0.05):</b> ", keputusan, "<br>",
                  "<b>Kesimpulan:</b> ", kesimpulan))
    }
  })
  
  # --- Logika Uji Varians --- #
  hasil_varians <- eventReactive(input$run_uji_varians, {
    req(input$var_uji_varians)
    data_var <- na.omit(sovi_data[[input$var_uji_varians]])
    
    if (input$tipe_uji_varians == "1 Kelompok") {
      req(input$nilai_sigma2)
      sampel <- sample(data_var, 100)
      # Menggunakan uji Chi-Square dari package EnvStats
      varTest(sampel, sigma.squared = input$nilai_sigma2, alternative = "two.sided")
    } else {
      n_total <- length(data_var)
      idx1 <- sample(1:n_total, 50)
      idx2 <- sample(setdiff(1:n_total, idx1), 50)
      
      df_uji <- data.frame(
        nilai = c(data_var[idx1], data_var[idx2]),
        kelompok = rep(c("Grup 1", "Grup 2"), each = 50)
      )
      
      list(
        levene = leveneTest(nilai ~ kelompok, data = df_uji),
        bartlett = bartlett.test(nilai ~ kelompok, data = df_uji)
      )
    }
  })
  
  output$hasil_uji_varians <- renderPrint({ hasil_varians() })
  output$interpretasi_uji_varians <- renderUI({
    res <- hasil_varians()
    alpha <- 0.05
    
    if (input$tipe_uji_varians == "1 Kelompok") {
      keputusan <- ifelse(res$p.value < alpha, "Menolak H₀", "Gagal Menolak H₀")
      kesimpulan <- ifelse(res$p.value < alpha,
                           "Terdapat cukup bukti untuk menyatakan varians sampel secara signifikan berbeda dari nilai hipotesis.",
                           "Tidak terdapat cukup bukti untuk menyatakan varians sampel berbeda dari nilai hipotesis.")
      HTML(paste0("<b>Hipotesis:</b> H₀: σ² = ", input$nilai_sigma2, " vs H₁: σ² ≠ ", input$nilai_sigma2, "<br>",
                  "<b>P-value:</b> ", round(res$p.value, 5), "<br>",
                  "<b>Keputusan (α=0.05):</b> ", keputusan, "<br>",
                  "<b>Kesimpulan:</b> ", kesimpulan))
    } else {
      # Interpretasi Levene
      p_levene <- res$levene$`Pr(>F)`[1]
      kep_levene <- ifelse(p_levene < alpha, "Menolak H₀ (Varians tidak sama)", "Gagal Menolak H₀ (Varians sama)")
      
      # Interpretasi Bartlett
      p_bartlett <- res$bartlett$p.value
      kep_bartlett <- ifelse(p_bartlett < alpha, "Menolak H₀ (Varians tidak sama)", "Gagal Menolak H₀ (Varians sama)")
      
      HTML(paste0("<b>Hipotesis:</b> H₀: σ₁² = σ₂² (Varians kedua kelompok sama) vs H₁: σ₁² ≠ σ₂²<br><br>",
                  "<b>Hasil Uji Levene:</b><br>",
                  "P-value: ", round(p_levene, 5), "<br>",
                  "Keputusan: ", kep_levene, "<br><br>",
                  "<b>Hasil Uji Bartlett:</b><br>",
                  "P-value: ", round(p_bartlett, 5), "<br>",
                  "Keputusan: ", kep_bartlett, "<br><br>",
                  "<i>Catatan: Uji Levene lebih direkomendasikan jika data tidak berdistribusi normal.</i>"))
    }
  })
  
  # --- ANOVA SATU ARAH --- #
  anova1_results <- eventReactive(input$run_anova1, {
    req(input$var_anova1_y, input$var_anova1_x)
    
    # Buat data frame kerja
    df_anova <- data.frame(
      y = sovi_data[[input$var_anova1_y]],
      x_cont = sovi_data[[input$var_anova1_x]]
    )
    
    # Buat faktor dengan 4 level
    df_anova$faktor <- cut(df_anova$x_cont, breaks = 4,
                           labels = c("Rendah", "Sedang", "Tinggi", "Sangat Tinggi"))
    
    # 1. Uji Asumsi Homogenitas
    levene_res <- leveneTest(y ~ faktor, data = df_anova)
    
    # 2. Uji Asumsi Normalitas
    model_initial <- aov(y ~ faktor, data = df_anova)
    shapiro_res <- shapiro.test(residuals(model_initial))
    
    # 3. Cek & Lakukan Transformasi jika perlu
    df_anova$y_final <- df_anova$y
    is_transformed <- FALSE
    if(shapiro_res$p.value < 0.05) {
      df_anova$y_final <- log(df_anova$y + 1) # Transformasi log(y+1)
      is_transformed <- TRUE
    }
    
    # 4. Jalankan ANOVA final (dengan data yang mungkin sudah ditransformasi)
    model_final <- aov(y_final ~ faktor, data = df_anova)
    anova_summary <- summary(model_final)
    
    # 5. Uji Lanjut Tukey HSD
    hsd_res <- HSD.test(model_final, "faktor", group = TRUE)
    
    # 6. Kembalikan semua hasil
    list(
      levene = levene_res,
      shapiro = shapiro_res,
      transformed = is_transformed,
      anova_summary = anova_summary,
      hsd = hsd_res,
      data_for_plot = df_anova,
      y_name = ifelse(is_transformed, paste0("log(", input$var_anova1_y, ")"), input$var_anova1_y),
      x_name = input$var_anova1_x
    )
  })
  
  # -- Output ANOVA 1 Arah -- #
  output$anova1_asumsi_homogen <- renderPrint({ anova1_results()$levene })
  output$anova1_asumsi_normal <- renderPrint({ anova1_results()$shapiro })
  output$anova1_plot_qq <- renderPlot({ qqPlot(residuals(aov(y ~ faktor, data = anova1_results()$data_for_plot))) })
  output$anova1_info_transformasi <- renderUI({
    if(anova1_results()$transformed) {
      tags$div(class = "alert alert-warning", role = "alert",
               "Karena asumsi normalitas tidak terpenuhi (p < 0.05), transformasi logaritmik diterapkan pada variabel dependen.")
    }
  })
  output$anova1_hasil_uji <- renderPrint({ anova1_results()$anova_summary })
  output$interpretasi_anova1_uji <- renderUI({
    res <- anova1_results()
    p_val <- res$anova_summary[[1]]$`Pr(>F)`[1]
    if(p_val < 0.05) {
      p("Hasil ANOVA menunjukkan bahwa nilai p < 0.05, artinya terdapat perbedaan rata-rata yang signifikan pada variabel dependen antar setidaknya dua kelompok faktor.")
    } else {
      p("Hasil ANOVA menunjukkan bahwa nilai p ≥ 0.05, artinya tidak terdapat perbedaan rata-rata yang signifikan antar kelompok.")
    }
  })
  output$anova1_hasil_lanjut <- renderPrint({ print(anova1_results()$hsd$groups) })
  output$interpretasi_anova1_lanjut <- renderUI({
    p("Tabel ini mengelompokkan level faktor. Kelompok yang tidak berbagi huruf yang sama (misal: 'a' dan 'b') memiliki rata-rata yang berbeda secara signifikan berdasarkan uji Tukey HSD.")
  })
  
  # --- ANOVA DUA ARAH --- #
  anova2_results <- eventReactive(input$run_anova2, {
    req(input$var_anova2_y, input$var_anova2_x1, input$var_anova2_x2)
    # Pastikan faktor tidak sama
    validate(need(input$var_anova2_x1 != input$var_anova2_x2, "Variabel untuk Faktor 1 dan Faktor 2 tidak boleh sama."))
    
    # Buat data frame kerja
    df_anova <- data.frame(
      y = sovi_data[[input$var_anova2_y]],
      x1_cont = sovi_data[[input$var_anova2_x1]],
      x2_cont = sovi_data[[input$var_anova2_x2]]
    )
    
    # Buat faktor
    df_anova$faktor1 <- cut(df_anova$x1_cont, breaks = 4, labels = paste0("F1.", c("R", "S", "T", "ST")))
    df_anova$faktor2 <- cut(df_anova$x2_cont, breaks = 4, labels = paste0("F2.", c("R", "S", "T", "ST")))
    
    # Jalankan ANOVA dan Asumsi (Transformasi tidak diimplementasikan di sini untuk menjaga keringkasan)
    model <- aov(y ~ faktor1 * faktor2, data = df_anova)
    
    list(
      levene = leveneTest(y ~ faktor1 * faktor2, data = df_anova),
      shapiro = shapiro.test(residuals(model)),
      anova_summary = summary(model),
      hsd = HSD.test(model, c("faktor1", "faktor2"), group = TRUE),
      data_for_plot = df_anova,
      y_name = input$var_anova2_y,
      x1_name = input$var_anova2_x1,
      x2_name = input$var_anova2_x2
    )
  })
  
  # -- Output ANOVA 2 Arah -- #
  output$anova2_asumsi_homogen <- renderPrint({ anova2_results()$levene })
  output$anova2_asumsi_normal <- renderPrint({ anova2_results()$shapiro })
  output$anova2_hasil_uji <- renderPrint({ anova2_results()$anova_summary })
  output$interpretasi_anova2_uji <- renderUI({
    res <- anova2_results()
    p_f1 <- res$anova_summary[[1]]$`Pr(>F)`[1]
    p_f2 <- res$anova_summary[[1]]$`Pr(>F)`[2]
    p_int <- res$anova_summary[[1]]$`Pr(>F)`[3]
    
    HTML(paste0("<ul>",
                "<li><b>Faktor 1 (", res$x1_name, "):</b> P-value = ", round(p_f1, 5), ". ", ifelse(p_f1 < 0.05, "Berpengaruh signifikan.", "Tidak berpengaruh signifikan."), "</li>",
                "<li><b>Faktor 2 (", res$x2_name, "):</b> P-value = ", round(p_f2, 5), ". ", ifelse(p_f2 < 0.05, "Berpengaruh signifikan.", "Tidak berpengaruh signifikan."), "</li>",
                "<li><b>Interaksi (Faktor1:Faktor2):</b> P-value = ", round(p_int, 5), ". ", ifelse(p_int < 0.05, "Terdapat efek interaksi yang signifikan.", "Tidak terdapat efek interaksi yang signifikan."), "</li>",
                "</ul>"))
  })
  
  output$anova2_hasil_lanjut <- renderPrint({ print(anova2_results()$hsd$groups) })
  output$anova2_plot_boxplot <- renderPlot({
    res <- anova2_results()
    ggplot(res$data_for_plot, aes(x = faktor1, y = y, fill = faktor2)) +
      geom_boxplot() +
      labs(title = paste("Boxplot", res$y_name, "berdasarkan", res$x1_name, "dan", res$x2_name),
           x = paste("Kelompok", res$x1_name), y = res$y_name, fill = paste("Kelompok", res$x2_name)) +
      theme_minimal()
  })
  
  # -- UI Dinamis untuk variabel X (Tidak Berubah) -- #
  output$variabel_x_ui <- renderUI({
    model_choice <- input$model_regresi
    
    default_vars <- switch(model_choice,
                           "Model 1: POVERTY ~ f(Pendidikan, Infrastruktur)" = c("NOELECTRIC", "ILLITERATE", "DPRONE"),
                           "Model 2: POPULATION ~ f(Demografi)" = c("FAMILYSIZE", "ELDERLY"),
                           "Model 3: POVERTY ~ f(Struktur Keluarga)" = c("ELDERLY", "FAMILYSIZE"),
                           "Model 4: LOWEDU ~ f(Kondisi Hidup)" = c("NOELECTRIC", "RENTED", "POVERTY", "NOSEWER"),
                           "Model 5: NOTRAINING ~ f(Kerentanan & Paparan)" = c("POVERTY", "LOWEDU", "DPRONE", "ELDERLY")
    )
    
    updatePickerInput(session, "additional_vars_x", selected = default_vars)
    
    pickerInput(
      inputId = "additional_vars_x",
      label = "2. Tambah/Kurangi Variabel Independen (X):", 
      choices = vars_for_assumption,
      selected = default_vars,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `size`=10)
    )
  })
  
  # -- Reactive UTAMA untuk Menjalankan Model AWAL dan Uji Asumsi -- #
  reg_model_initial <- eventReactive(input$run_regresi, {
    req(input$model_regresi, input$additional_vars_x)
    
    y_var <- switch(input$model_regresi,
                    "Model 1: POVERTY ~ f(Pendidikan, Infrastruktur)" = "POVERTY",
                    "Model 2: POPULATION ~ f(Demografi)" = "POPULATION",
                    "Model 3: POVERTY ~ f(Struktur Keluarga)" = "POVERTY",
                    "Model 4: LOWEDU ~ f(Kondisi Hidup)" = "LOWEDU",
                    "Model 5: NOTRAINING ~ f(Kerentanan & Paparan)" = "NOTRAINING"
    )
    x_vars <- input$additional_vars_x
    
    validate(need(!y_var %in% x_vars, paste("Variabel dependen (", y_var, ") tidak boleh dipilih sebagai variabel independen.")))
    if(length(x_vars) == 0) return(NULL)
    
    formula <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
    model <- lm(formula, data = sovi_data)
    
    # Langsung jalankan semua uji asumsi di sini
    bp_test <- bptest(model)
    sw_test <- shapiro.test(residuals(model))
    dw_test <- dwtest(model)
    vif_test <- if (length(coef(model)) > 2) vif(model) else NULL
    
    # Kembalikan semua hasil dalam satu list yang rapi
    list(
      model = model,
      y_var = y_var,
      x_vars = x_vars,
      assumptions = list(
        bp = bp_test,
        sw = sw_test,
        dw = dw_test,
        vif = vif_test
      )
    )
  })
  
  # -- Output Summary Model Awal -- #
  output$summary_regresi <- renderPrint({ 
    req(reg_model_initial())
    summary(reg_model_initial()$model) 
  })
  
  # --- observeEvent untuk MENYIMPAN status asumsi --- #
  observeEvent(reg_model_initial(), {
    req(reg_model_initial())
    ar <- reg_model_initial()$assumptions
    alpha <- 0.05
    
    homo_met <- ar$bp$p.value >= alpha
    norm_met <- ar$sw$p.value >= alpha
    auto_met <- ar$dw$p.value >= alpha
    multi_met <- ifelse(is.null(ar$vif), TRUE, !any(ar$vif >= 5))
    
    # Simpan status ke dalam reactiveVal
    assumption_status(list(homo = homo_met, norm = norm_met, auto = auto_met, multi = multi_met))
  })
  
  # -- Reactive Value untuk Menyimpan Status Asumsi (Letakkan di awal server) -- #
  assumption_status <- reactiveVal()
  
  # -- Output Summary Model Awal (Tidak perlu diubah, sudah benar) -- #
  output$summary_regresi <- renderPrint({ 
    req(reg_model_initial())
    summary(reg_model_initial()$model) 
  })
  
  output$interpretasi_regresi_summary <- renderUI({
    # Pastikan model sudah dijalankan sebelum menampilkan interpretasi
    req(reg_model_initial())
    
    # Ambil model dan summary-nya
    model <- reg_model_initial()$model
    s <- summary(model)
    
    # 1. Interpretasi Adjusted R-squared
    r2_interp <- paste0(
      "<b>Adjusted R-squared:</b> Nilai R² yang disesuaikan adalah ", round(s$adj.r.squared, 4), ". ",
      "Artinya, sekitar <b>", round(s$adj.r.squared * 100, 2), "%</b> variasi dari variabel dependen (Y) ",
      "dapat dijelaskan oleh variabel-variabel independen (X) yang ada di dalam model ini."
    )
    
    # 2. Interpretasi Uji F (Signifikansi Model Simultan)
    # Menghitung p-value dari F-statistic
    f_p_value <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
    f_interp <- paste0(
      "<b>Uji F-statistik:</b> Model ini secara keseluruhan <b>", 
      ifelse(f_p_value < 0.05, "signifikan", "tidak signifikan"), "</b>",
      " (p-value: ", format.pval(f_p_value, digits=4), "). ",
      "Ini menunjukkan bahwa secara bersama-sama, variabel-variabel independen memiliki pengaruh yang nyata terhadap variabel dependen."
    )
    
    # 3. Interpretasi Variabel Signifikan
    coef_df <- as.data.frame(s$coefficients)
    names(coef_df) <- c("Estimate", "StdError", "tValue", "PValue")
    # Abaikan intercept (baris pertama)
    coef_df <- coef_df[-1, , drop = FALSE]   
    
    # Cari variabel yang p-value nya < 0.05
    sig_vars <- coef_df[coef_df$PValue < 0.05, , drop = FALSE]
    
    if (nrow(sig_vars) > 0) {
      sig_vars_list <- lapply(rownames(sig_vars), function(var_name) {
        effect <- ifelse(sig_vars[var_name, "Estimate"] > 0, "positif", "negatif")
        p_val_text <- format.pval(sig_vars[var_name, "PValue"], digits=4)
        paste0("<li>Variabel <b>", var_name, "</b> berpengaruh signifikan secara statistik dengan efek <b>", effect, "</b> (p-value = ", p_val_text, ").</li>")
      })
      sig_vars_interp <- paste0("<ul>", paste(unlist(sig_vars_list), collapse = ""), "</ul>")
    } else {
      sig_vars_interp <- "<p>Tidak ada variabel independen yang berpengaruh signifikan secara individual pada tingkat signifikansi 5%.</p>"
    }
    
    # Gabungkan semua interpretasi menjadi satu output HTML
    HTML(paste(
      "<h5><i class='fa fa-lightbulb-o'></i> Insight Kunci dari Model</h5>",
      "<p>", r2_interp, "</p>",
      "<p>", f_interp, "</p>",
      "<h5>Variabel yang Berpengaruh Signifikan (α = 5%):</h5>",
      sig_vars_interp
    ))
  })
  
  # --- BLOK BARU: eventReactive terpusat untuk MENGHITUNG status asumsi --- #
  assumption_status_reactive <- eventReactive(reg_model_initial(), {
    # Blok ini berjalan HANYA jika model awal berubah
    req(reg_model_initial())
    ar <- reg_model_initial()$assumptions
    alpha <- 0.05
    
    # Lakukan semua perhitungan di sini
    homo_met <- ar$bp$p.value >= alpha
    norm_met <- ar$sw$p.value >= alpha
    auto_met <- ar$dw$p.value >= alpha
    multi_met <- ifelse(is.null(ar$vif), TRUE, !any(ar$vif >= 5))
    
    # Kembalikan hasil status dalam bentuk list
    list(homo = homo_met, norm = norm_met, auto = auto_met, multi = multi_met)
  })
  
  # --- BLOK BARU: observeEvent untuk MENYIMPAN status asumsi --- #
  observeEvent(assumption_status_reactive(), {
    # Simpan hasil dari reactive di atas ke dalam reactiveVal
    # agar bisa diakses oleh UI Perbaikan Model
    assumption_status(assumption_status_reactive())
  })
  
  # -- Output Ringkasan Asumsi AWAL (DISEDERHANAKAN) -- #
  output$summary_assumptions <- renderUI({
    # Blok ini sekarang hanya bertugas MENAMPILKAN, bukan menghitung
    req(assumption_status_reactive())
    status <- assumption_status_reactive() # Ambil status yang sudah dihitung
    
    all_assumptions <- unlist(status)
    met_count <- sum(all_assumptions, na.rm = TRUE)
    total_count <- length(all_assumptions)
    
    # Teks Kesimpulan
    if (met_count == total_count) {
      kesimpulan <- "🎉 Model regresi memenuhi semua asumsi klasik.\n   Penaksir yang dihasilkan bersifat BLUE (Best Linear Unbiased Estimator).\n   Model dapat digunakan untuk inferensi dan prediksi dengan tingkat kepercayaan tinggi."
      rekomendasi <- "• Model sudah optimal!"
    } else {
      kesimpulan <- "Model belum memenuhi semua asumsi klasik dan mungkin menghasilkan penaksir yang bias."
      rekomendasi <- "• Pertimbangkan untuk melakukan transformasi data, menambah/mengurangi variabel, atau menggunakan metode regresi lain."
    }
    
    # Tampilan output (tidak ada perubahan di sini)
    tags$pre(
      paste(
        "RINGKASAN HASIL PENGUJIAN ASUMSI KLASIK MODEL REGRESI\n",
        "================================================================\n\n",
        sprintf("  Homoskedastisitas        : %s %s\n", ifelse(status$homo, "✓", "✗"), ifelse(status$homo, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Normalitas Residual      : %s %s\n", ifelse(status$norm, "✓", "✗"), ifelse(status$norm, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Non-multikolinearitas    : %s %s\n", ifelse(status$multi, "✓", "✗"), ifelse(status$multi, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Non-autokorelasi         : %s %s\n\n", ifelse(status$auto, "✓", "✗"), ifelse(status$auto, "TERPENUHI", "TIDAK TERPENUHI")),
        "================================================================\n",
        sprintf("KESIMPULAN: Model memenuhi %d dari %d asumsi klasik (%.0f%%).\n\n", met_count, total_count, (met_count/total_count)*100),
        paste0(kesimpulan, "\n\n"),
        "================================================================\n",
        "REKOMENDASI:\n",
        rekomendasi
      )
    )
  })
  
  # --- UI DINAMIS UNTUK BOX PERBAIKAN --- #
  output$ui_perbaikan_asumsi <- renderUI({
    req(assumption_status())
    status <- assumption_status()
    
    # Jika tidak semua asumsi terpenuhi, tampilkan box perbaikan
    if (!all(unlist(status))) {
      box(width = 12, title = "Perbaikan Model", status = "danger", solidHeader = TRUE,
          p("Beberapa asumsi klasik tidak terpenuhi. Anda dapat mencoba memperbaiki model."),
          checkboxGroupInput("pilihan_perbaikan", "Pilih Perbaikan yang Akan Dilakukan:",
                             choices = c(
                               # Label diubah menjadi Transformasi Box-Cox
                               "Normalitas (Transformasi Box-Cox)"[!status$norm],
                               "Homoskedastisitas (Regresi WLS)"[!status$homo],
                               "Autokorelasi (Metode Selisih)"[!status$auto]
                             )
          ),
          actionButton("run_perbaikan", "Jalankan Perbaikan Model", icon = icon("wrench"))
      )
    }
  })
  
  # --- LOGIKA PERBAIKAN MODEL --- #
  # --- UI DINAMIS UNTUK BOX PERBAIKAN --- #
  output$ui_perbaikan_asumsi <- renderUI({
    req(assumption_status())
    status <- assumption_status()
    
    # Jika tidak semua asumsi terpenuhi, tampilkan box perbaikan
    if (!all(unlist(status))) {
      box(width = 12, title = "Perbaikan Model", status = "danger", solidHeader = TRUE,
          p("Beberapa asumsi klasik tidak terpenuhi. Anda dapat mencoba memperbaiki model."),
          # Buat checkbox untuk perbaikan yang tersedia
          checkboxGroupInput("pilihan_perbaikan", "Pilih Perbaikan yang Akan Dilakukan:",
                             choices = c(
                               "Normalitas (Transformasi Box-COX)"[!status$norm],
                               "Homoskedastisitas (Regresi WLS)"[!status$homo],
                               "Autokorelasi (Metode Selisih)"[!status$auto]
                             )
          ),
          actionButton("run_perbaikan", "Jalankan Perbaikan Model", icon = icon("wrench"))
      )
    }
  })
  
  # --- LOGIKA PERBAIKAN MODEL --- #
  reg_model_fixed <- eventReactive(input$run_perbaikan, {
    req(reg_model_initial(), input$pilihan_perbaikan)
    
    initial_res <- reg_model_initial()
    y_var <- initial_res$y_var
    x_vars <- initial_res$x_vars
    temp_data <- sovi_data
    
    fixes_applied <- list()
    
    # Perbaikan Normalitas (Transformasi Box-Cox)
    if ("Normalitas (Transformasi Box-Cox)" %in% input$pilihan_perbaikan) {
      
      # Syarat Box-Cox: Variabel Y harus > 0
      if (any(temp_data[[y_var]] <= 0, na.rm = TRUE)) {
        
        # Tampilkan notifikasi error kepada pengguna
        showNotification(
          "Transformasi Box-Cox gagal: Variabel dependen (Y) harus berisi nilai positif.",
          type = "error", duration = 10
        )
        fixes_applied$norm_failed <- "Normalitas: Perbaikan Box-Cox tidak dapat dilakukan karena data Y mengandung nilai non-positif."
        
      } else {
        
        # Buat model OLS sementara hanya untuk menemukan lambda
        formula_temp <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
        model_temp_for_bc <- lm(formula_temp, data = temp_data)
        
        # Cari lambda optimal tanpa menampilkan plot di konsol
        bc_result <- boxcox(model_temp_for_bc, plotit = FALSE)
        lambda <- bc_result$x[which.max(bc_result$y)]
        
        # Terapkan transformasi Box-Cox ke variabel Y
        # Jika lambda mendekati 0, transformasinya setara dengan log(Y)
        if (abs(lambda) < 0.01) {
          temp_data[[y_var]] <- log(temp_data[[y_var]])
        } else {
          temp_data[[y_var]] <- (temp_data[[y_var]]^lambda - 1) / lambda
        }
        
        fixes_applied$norm <- paste0("Normalitas: Transformasi Box-Cox diterapkan pada variabel Y (lambda optimal ≈ ", round(lambda, 2), ").")
      }
    }
    
    # Perbaikan Autokorelasi (Metode Selisih)
    if ("Autokorelasi (Metode Selisih)" %in% input$pilihan_perbaikan) {
      temp_data <- temp_data %>%
        arrange(NO) %>% 
        mutate(across(c(y_var, all_of(x_vars)), ~ . - lag(.), .names = "{.col}_diff"))
      
      y_var <- paste0(y_var, "_diff")
      x_vars <- paste0(x_vars, "_diff")
      fixes_applied$auto <- "Autokorelasi: Model diestimasi ulang menggunakan metode First Difference."
      
      temp_data <- na.omit(temp_data)
    }
    
    formula_fixed <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
    
    wls_applied <- "Homoskedastisitas (Regresi WLS)" %in% input$pilihan_perbaikan
    
    # Perbaikan Homoskedastisitas (WLS)
    if (wls_applied) {
      model_ols <- lm(formula_fixed, data = temp_data)
      df_for_wls <- data.frame(
        abs_res = abs(residuals(model_ols)),
        predictor = model.frame(model_ols)[[x_vars[1]]]
      )
      abs_res_model <- lm(abs_res ~ predictor, data = df_for_wls)
      bobot <- 1 / (fitted(abs_res_model)^2)
      model_fixed <- lm(formula_fixed, data = temp_data, weights = bobot)
      fixes_applied$homo <- "Homoskedastisitas: Model diestimasi ulang menggunakan Weighted Least Square (WLS)."
    } else {
      model_fixed <- lm(formula_fixed, data = temp_data)
    }
    
    # Uji kembali asumsi pada model yang sudah diperbaiki
    resids_fixed <- residuals(model_fixed)
    sw_test_fixed <- if(length(resids_fixed) > 2) shapiro.test(resids_fixed) else list(p.value=NA, statistic=NA, method="Shapiro-Wilk Test Not Applicable")
    vif_test_fixed <- if (length(coef(model_fixed)) > 2) tryCatch(vif(model_fixed), error=function(e) NULL) else NULL
    
    if (wls_applied) {
      bp_test_fixed <- list(statistic = NA, p.value = 1, method = "Breusch-Pagan test bypassed: WLS correction applied.")
      dw_test_fixed <- tryCatch(dwtest(model_fixed), error = function(e) {
        list(statistic = NA, p.value = 1, method = "Durbin-Watson test not supported for this model type.")
      })
    } else {
      bp_test_fixed <- bptest(model_fixed)
      dw_test_fixed <- dwtest(model_fixed)
    }
    
    list(
      model = model_fixed,
      fixes = fixes_applied,
      assumptions = list(
        bp = bp_test_fixed,
        sw = sw_test_fixed,
        dw = dw_test_fixed,
        vif = vif_test_fixed
      )
    )
  })
  
  # --- UI DINAMIS UNTUK HASIL PERBAIKAN --- #
  output$ui_hasil_perbaikan <- renderUI({
    # Hanya tampilkan box ini setelah tombol perbaikan ditekan
    req(reg_model_fixed())   
    
    res <- reg_model_fixed()
    
    # Buat ringkasan perbaikan yang dilakukan
    summary_fixes <- paste("<li>", unlist(res$fixes), "</li>", collapse = "")
    
    fluidRow(
      box(width = 12, title = "Hasil Model Setelah Perbaikan", status = "info", solidHeader = TRUE,
          h4("Tindakan Perbaikan yang Dilakukan:"),
          HTML(paste0("<ul>", summary_fixes, "</ul>")),
          hr(),
          h4("Summary Model Baru:"),
          verbatimTextOutput("summary_regresi_fixed")
      ),
      box(width = 12, title = "Pemeriksaan Ulang Asumsi Klasik", status = "success", solidHeader = TRUE,
          uiOutput("summary_assumptions_fixed")
      )
    )
  })
  
  # --- FUNGSI untuk Membuat Tampilan Ringkasan Asumsi --- #
  buat_ringkasan_asumsi <- function(assumption_results) {
    ar <- assumption_results
    alpha <- 0.05
    
    homo_met <- ar$bp$p.value >= alpha
    norm_met <- ar$sw$p.value >= alpha
    auto_met <- ar$dw$p.value >= alpha
    multi_met <- ifelse(is.null(ar$vif), TRUE, !any(ar$vif >= 5))
    
    all_assumptions <- c(homo_met, norm_met, multi_met, auto_met)
    met_count <- sum(all_assumptions, na.rm = TRUE)
    total_count <- length(all_assumptions)
    
    if (met_count == total_count) {
      kesimpulan <- "🎉 Model regresi memenuhi semua asumsi klasik.\n   Penaksir yang dihasilkan bersifat BLUE (Best Linear Unbiased Estimator).\n   Model dapat digunakan untuk inferensi dan prediksi dengan tingkat kepercayaan tinggi."
      rekomendasi <- "• Model sudah optimal!"
    } else {
      kesimpulan <- "Model belum memenuhi semua asumsi klasik dan mungkin menghasilkan penaksir yang bias."
      rekomendasi <- "• Pertimbangkan untuk melakukan transformasi data, menambah/mengurangi variabel, atau menggunakan metode regresi lain."
    }
    
    # Tampilan output (tidak ada perubahan di sini)
    tags$pre(
      paste(
        "RINGKASAN HASIL PENGUJIAN ASUMSI KLASIK MODEL REGRESI\n",
        "================================================================\n\n",
        sprintf("  Homoskedastisitas        : %s %s\n", ifelse(status$homo, "✓", "✗"), ifelse(status$homo, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Normalitas Residual      : %s %s\n", ifelse(status$norm, "✓", "✗"), ifelse(status$norm, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Non-multikolinearitas    : %s %s\n", ifelse(status$multi, "✓", "✗"), ifelse(status$multi, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Non-autokorelasi         : %s %s\n\n", ifelse(status$auto, "✓", "✗"), ifelse(status$auto, "TERPENUHI", "TIDAK TERPENUHI")),
        "================================================================\n",
        sprintf("KESIMPULAN: Model memenuhi %d dari %d asumsi klasik (%.0f%%).\n\n", met_count, total_count, (met_count/total_count)*100),
        paste0(kesimpulan, "\n\n"),
        "================================================================\n",
        "REKOMENDASI:\n",
        rekomendasi
      )
    )
  }
  
  # --- UI DINAMIS UNTUK BOX PERBAIKAN --- #
  output$ui_perbaikan_asumsi <- renderUI({
    req(assumption_status())
    status <- assumption_status()
    
    # Jika tidak semua asumsi terpenuhi, tampilkan box perbaikan
    if (!all(unlist(status))) {
      box(width = 12, title = "Perbaikan Model", status = "danger", solidHeader = TRUE,
          p("Beberapa asumsi klasik tidak terpenuhi. Anda dapat mencoba memperbaiki model."),
          checkboxGroupInput("pilihan_perbaikan", "Pilih Perbaikan yang Akan Dilakukan:",
                             choices = c(
                               # Label diubah menjadi Transformasi Box-Cox
                               "Normalitas (Transformasi Box-Cox)"[!status$norm],
                               "Homoskedastisitas (Regresi WLS)"[!status$homo],
                               "Autokorelasi (Metode Selisih)"[!status$auto]
                             )
          ),
          actionButton("run_perbaikan", "Jalankan Perbaikan Model", icon = icon("wrench"))
      )
    }
  })
  
  # --- LOGIKA PERBAIKAN MODEL --- #
  # --- UI DINAMIS UNTUK BOX PERBAIKAN --- #
  output$ui_perbaikan_asumsi <- renderUI({
    req(assumption_status())
    status <- assumption_status()
    
    # Jika tidak semua asumsi terpenuhi, tampilkan box perbaikan
    if (!all(unlist(status))) {
      box(width = 12, title = "Perbaikan Model", status = "danger", solidHeader = TRUE,
          p("Beberapa asumsi klasik tidak terpenuhi. Anda dapat mencoba memperbaiki model."),
          # Buat checkbox untuk perbaikan yang tersedia
          checkboxGroupInput("pilihan_perbaikan", "Pilih Perbaikan yang Akan Dilakukan:",
                             choices = c(
                               "Normalitas (Transformasi Box-COX)"[!status$norm],
                               "Homoskedastisitas (Regresi WLS)"[!status$homo],
                               "Autokorelasi (Metode Selisih)"[!status$auto]
                             )
          ),
          actionButton("run_perbaikan", "Jalankan Perbaikan Model", icon = icon("wrench"))
      )
    }
  })
  
  # --- LOGIKA PERBAIKAN MODEL --- #
  reg_model_fixed <- eventReactive(input$run_perbaikan, {
    req(reg_model_initial(), input$pilihan_perbaikan)
    
    
    initial_res <- reg_model_initial()
    y_var <- initial_res$y_var
    x_vars <- initial_res$x_vars
    temp_data <- sovi_data
    
    fixes_applied <- list()
    
    # Perbaikan Normalitas (Transformasi Box-Cox)
    if ("Normalitas (Transformasi Box-Cox)" %in% input$pilihan_perbaikan) {
      
      # Syarat Box-Cox: Variabel Y harus > 0
      if (any(temp_data[[y_var]] <= 0, na.rm = TRUE)) {
        
        # Tampilkan notifikasi error kepada pengguna
        showNotification(
          "Transformasi Box-Cox gagal: Variabel dependen (Y) harus berisi nilai positif.",
          type = "error", duration = 10
        )
        fixes_applied$norm_failed <- "Normalitas: Perbaikan Box-Cox tidak dapat dilakukan karena data Y mengandung nilai non-positif."
        
      } else {
        
        # Buat model OLS sementara hanya untuk menemukan lambda
        formula_temp <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
        model_temp_for_bc <- lm(formula_temp, data = temp_data)
        
        # Cari lambda optimal tanpa menampilkan plot di konsol
        bc_result <- boxcox(model_temp_for_bc, plotit = FALSE)
        lambda <- bc_result$x[which.max(bc_result$y)]
        
        # Terapkan transformasi Box-Cox ke variabel Y
        # Jika lambda mendekati 0, transformasinya setara dengan log(Y)
        if (abs(lambda) < 0.01) {
          temp_data[[y_var]] <- log(temp_data[[y_var]])
        } else {
          temp_data[[y_var]] <- (temp_data[[y_var]]^lambda - 1) / lambda
        }
        
        fixes_applied$norm <- paste0("Normalitas: Transformasi Box-Cox diterapkan pada variabel Y (lambda optimal ≈ ", round(lambda, 2), ").")
      }
    }
    
    # Perbaikan Autokorelasi (Metode Selisih)
    if ("Autokorelasi (Metode Selisih)" %in% input$pilihan_perbaikan) {
      temp_data <- temp_data %>%
        arrange(NO) %>% 
        mutate(across(c(y_var, all_of(x_vars)), ~ . - lag(.), .names = "{.col}_diff"))
      
      y_var <- paste0(y_var, "_diff")
      x_vars <- paste0(x_vars, "_diff")
      fixes_applied$auto <- "Autokorelasi: Model diestimasi ulang menggunakan metode First Difference."
      
      temp_data <- na.omit(temp_data)
    }
    
    formula_fixed <- as.formula(paste(y_var, "~", paste(x_vars, collapse = " + ")))
    
    wls_applied <- "Homoskedastisitas (Regresi WLS)" %in% input$pilihan_perbaikan
    
    # Perbaikan Homoskedastisitas (WLS)
    if (wls_applied) {
      model_ols <- lm(formula_fixed, data = temp_data)
      df_for_wls <- data.frame(
        abs_res = abs(residuals(model_ols)),
        predictor = model.frame(model_ols)[[x_vars[1]]]
      )
      abs_res_model <- lm(abs_res ~ predictor, data = df_for_wls)
      bobot <- 1 / (fitted(abs_res_model)^2)
      model_fixed <- lm(formula_fixed, data = temp_data, weights = bobot)
      fixes_applied$homo <- "Homoskedastisitas: Model diestimasi ulang menggunakan Weighted Least Square (WLS)."
    } else {
      model_fixed <- lm(formula_fixed, data = temp_data)
    }
    
    # Uji kembali asumsi pada model yang sudah diperbaiki
    resids_fixed <- residuals(model_fixed)
    sw_test_fixed <- if(length(resids_fixed) > 2) shapiro.test(resids_fixed) else list(p.value=NA, statistic=NA, method="Shapiro-Wilk Test Not Applicable")
    vif_test_fixed <- if (length(coef(model_fixed)) > 2) tryCatch(vif(model_fixed), error=function(e) NULL) else NULL
    
    if (wls_applied) {
      bp_test_fixed <- list(statistic = NA, p.value = 1, method = "Breusch-Pagan test bypassed: WLS correction applied.")
      dw_test_fixed <- tryCatch(dwtest(model_fixed), error = function(e) {
        list(statistic = NA, p.value = 1, method = "Durbin-Watson test not supported for this model type.")
      })
    } else {
      bp_test_fixed <- bptest(model_fixed)
      dw_test_fixed <- dwtest(model_fixed)
    }
    
    list(
      model = model_fixed,
      fixes = fixes_applied,
      assumptions = list(
        bp = bp_test_fixed,
        sw = sw_test_fixed,
        dw = dw_test_fixed,
        vif = vif_test_fixed
      )
    )
  })
  
  # --- UI DINAMIS UNTUK HASIL PERBAIKAN --- #
  output$ui_hasil_perbaikan <- renderUI({
    # Hanya tampilkan box ini setelah tombol perbaikan ditekan
    req(reg_model_fixed()) 
    
    res <- reg_model_fixed()
    
    # Buat ringkasan perbaikan yang dilakukan
    summary_fixes <- paste("<li>", unlist(res$fixes), "</li>", collapse = "")
    
    fluidRow(
      box(width = 12, title = "Hasil Model Setelah Perbaikan", status = "info", solidHeader = TRUE,
          h4("Tindakan Perbaikan yang Dilakukan:"),
          HTML(paste0("<ul>", summary_fixes, "</ul>")),
          hr(),
          h4("Summary Model Baru:"),
          verbatimTextOutput("summary_regresi_fixed")
      ),
      box(width = 12, title = "Pemeriksaan Ulang Asumsi Klasik", status = "success", solidHeader = TRUE,
          uiOutput("summary_assumptions_fixed")
      )
    )
  })
  
  # --- FUNGSI untuk Membuat Tampilan Ringkasan Asumsi --- #
  buat_ringkasan_asumsi <- function(assumption_results) {
    ar <- assumption_results
    alpha <- 0.05
    
    homo_met <- ar$bp$p.value >= alpha
    norm_met <- ar$sw$p.value >= alpha
    auto_met <- ar$dw$p.value >= alpha
    multi_met <- ifelse(is.null(ar$vif), TRUE, !any(ar$vif >= 5))
    
    all_assumptions <- c(homo_met, norm_met, multi_met, auto_met)
    met_count <- sum(all_assumptions, na.rm = TRUE)
    total_count <- length(all_assumptions)
    
    if (met_count == total_count) {
      kesimpulan <- "🎉 Model regresi memenuhi semua asumsi klasik.\n   Penaksir yang dihasilkan bersifat BLUE (Best Linear Unbiased Estimator).\n   Model dapat digunakan untuk inferensi dan prediksi dengan tingkat kepercayaan tinggi."
      rekomendasi <- "• Model sudah optimal!"
    } else {
      kesimpulan <- "Model belum memenuhi semua asumsi klasik dan mungkin menghasilkan penaksir yang bias."
      rekomendasi <- "• Pertimbangkan untuk melakukan transformasi data, menambah/mengurangi variabel, atau menggunakan metode regresi lain."
    }
    
    tags$pre(
      paste(
        "RINGKASAN HASIL PENGUJIAN ASUMSI KLASIK MODEL REGRESI\n",
        "================================================================\n\n",
        sprintf("  Homoskedastisitas        : %s %s\n", ifelse(homo_met, "✓", "✗"), ifelse(homo_met, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Normalitas Residual      : %s %s\n", ifelse(norm_met, "✓", "✗"), ifelse(norm_met, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Non-multikolinearitas    : %s %s\n", ifelse(multi_met, "✓", "✗"), ifelse(multi_met, "TERPENUHI", "TIDAK TERPENUHI")),
        sprintf("  Non-autokorelasi         : %s %s\n\n", ifelse(auto_met, "✓", "✗"), ifelse(auto_met, "TERPENUHI", "TIDAK TERPENUHI")),
        "================================================================\n",
        sprintf("KESIMPULAN: Model memenuhi %d dari %d asumsi klasik (%.0f%%).\n\n", met_count, total_count, (met_count/total_count)*100),
        paste0(kesimpulan, "\n\n"),
        "================================================================\n",
        "REKOMENDASI:\n",
        rekomendasi
      )
    )
  }
  
  # -- Output Ringkasan Asumsi AWAL -- #
  output$summary_assumptions <- renderUI({
    req(reg_model_initial())
    buat_ringkasan_asumsi(reg_model_initial()$assumptions)
  })
  
  # -- Output untuk hasil yang sudah diperbaiki -- #
  output$summary_regresi_fixed <- renderPrint({ 
    req(reg_model_fixed())
    summary(reg_model_fixed()$model) 
  })
  
  output$summary_assumptions_fixed <- renderUI({
    req(reg_model_fixed())
    buat_ringkasan_asumsi(reg_model_fixed()$assumptions)
  })
  
  output$download_laporan_regresi <- downloadHandler(
    filename = function() {
      paste0("laporan_regresi_lengkap_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Pastikan analisis awal sudah dijalankan
      req(reg_model_initial())
      
      withProgress(message = 'Membuat laporan regresi...', value = 0, {
        
        # --- Mengumpulkan Data dari Model AWAL ---
        incProgress(0.2, detail = "Mengumpulkan hasil awal...")
        
        initial_model_summary <- capture.output(summary(reg_model_initial()$model))
        
        # Menggunakan fungsi buat_ringkasan_asumsi untuk mengambil teks ringkasan
        # Perlu sedikit trik untuk menangkap output dari tags$pre
        initial_assumptions_ui <- buat_ringkasan_asumsi(reg_model_initial()$assumptions)
        initial_assumptions_text <- as.character(initial_assumptions_ui$children[[1]])
        
        
        # --- Siapkan Parameter Awal ---
        params_to_pass <- list(
          model_name = input$model_regresi,
          initial_summary_text = paste(initial_model_summary, collapse = "\n"),
          initial_assumptions_text = initial_assumptions_text,
          fixes_applied_list = NULL,
          fixed_summary_text = NULL,
          fixed_assumptions_text = NULL
        )
        
        # --- Cek dan Kumpulkan Data dari Model PERBAIKAN (jika ada) ---
        # `try()` digunakan agar tidak error jika model perbaikan belum dijalankan
        fixed_model_results <- try(reg_model_fixed(), silent = TRUE)
        
        if (!inherits(fixed_model_results, "try-error") && !is.null(fixed_model_results)) {
          incProgress(0.6, detail = "Mengumpulkan hasil perbaikan...")
          
          params_to_pass$fixes_applied_list <- unlist(fixed_model_results$fixes)
          
          fixed_model_summary <- capture.output(summary(fixed_model_results$model))
          params_to_pass$fixed_summary_text <- paste(fixed_model_summary, collapse = "\n")
          
          fixed_assumptions_ui <- buat_ringkasan_asumsi(fixed_model_results$assumptions)
          params_to_pass$fixed_assumptions_text <- as.character(fixed_assumptions_ui$children[[1]])
        }
        
        # --- Render Laporan ---
        incProgress(0.8, detail = "Menyusun dokumen...")
        rmarkdown::render(
          "template/laporan_regresi_lengkap.Rmd",
          output_file = file,
          params = params_to_pass,
          envir = new.env(parent = globalenv())
        )
        incProgress(1)
      })
    }
  )
} # Akhir dari server

# Menjalankan aplikasi
shinyApp(ui, server)