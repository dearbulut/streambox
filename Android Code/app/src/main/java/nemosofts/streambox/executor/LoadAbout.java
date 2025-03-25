plugins {
    id 'com.onesignal.androidsdk.onesignal-gradle-plugin'
    id 'com.android.application'
    id 'com.google.gms.google-services'
}

android {
    namespace 'nemosofts.streambox'
    compileSdk 35

    defaultConfig {
        applicationId "bulutsoft.dengeserver"
        minSdk 23
        targetSdk 35
        versionCode 3
        versionName "3.3.0"

        testInstrumentationRunner "androidx.test.runner.AndroidJUnitRunner"
        multiDexEnabled true
    }

    buildTypes {
        release {
            minifyEnabled true
            proguardFiles getDefaultProguardFile('proguard-android-optimize.txt'), 'proguard-rules.pro'
        }
    }

    buildTypes.each {
        it.buildConfigField 'String', 'BASE_URL', BASE_URL
        it.buildConfigField 'String', 'API_NAME', API_NAME
        it.buildConfigField 'String', 'ENC_KEY', ENC_KEY
        it.buildConfigField 'String', 'IV', IV
    }

    compileOptions {
        sourceCompatibility JavaVersion.VERSION_17
        targetCompatibility JavaVersion.VERSION_17
    }

    buildFeatures {
        buildConfig = true
    }

    lint {
        disable 'OldTargetApi', 'GradleDependency', 'GradleDynamicVersion'
    }
}

dependencies {
    implementation libs.bundles.nemosofts
    implementation libs.bundles.network.security
    implementation libs.androidx.multidex
    implementation libs.recyclerview
    implementation libs.androidx.palette
    implementation libs.scytale
    implementation libs.commons.io
    implementation(platform(libs.firebase.bom))
    implementation libs.bundles.firebase
    implementation libs.oneSignal
    implementation libs.androidx.activity
    implementation libs.bundles.youtubeplayer
    implementation libs.snowfall
    implementation libs.admob
    implementation libs.bundles.media3
    implementation(libs.bundles.exoplayer) {
        exclude group: "androidx.media3", module: "media3-exoplayer"
    }
    implementation fileTree(dir: "libs", include: ["lib-*.aar"])
}
