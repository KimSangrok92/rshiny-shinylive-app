# Shinylive + GitHub Pages 배포

이 폴더는 `app.R`를 Shinylive로 변환해 GitHub Pages로 배포하도록 설정되어 있습니다.

## 포함된 파일
- `app.R`: 배포용 Shiny 앱 엔트리 파일
- `.github/workflows/deploy-shinylive.yml`: GitHub Actions 배포 워크플로
- `.gitignore`: `_site/` 등 로컬 산출물 제외

## 1) GitHub 저장소에 업로드
아직 git 저장소가 아니면 아래를 실행하세요.

```powershell
git init
git add .
git commit -m "Add Shinylive deployment setup"
git branch -M main
git remote add origin https://github.com/<YOUR_ID>/<YOUR_REPO>.git
git push -u origin main
```

## 2) GitHub Pages 설정
GitHub 저장소 > `Settings` > `Pages`
- `Build and deployment`의 Source를 `GitHub Actions`로 설정

## 3) 배포 확인
- `Actions` 탭에서 `Deploy Shinylive to Pages` 워크플로가 성공하면 배포됨
- URL: `https://<YOUR_ID>.github.io/<YOUR_REPO>/`

## 로컬 빌드 테스트 (선택)
```powershell
D:\download\codex rshiny\r\R\R-4.5.2\bin\Rscript.exe -e "shinylive::export('.', '_site')"
```

## 주의
현재 앱은 의존 패키지가 많아 export 결과(`_site/app.json`)가 매우 커질 수 있습니다.
GitHub Pages 배포가 실패하면, 앱 의존 패키지를 줄이거나 서버형 Shiny 배포(Shiny Server/Posit Connect)를 검토해야 합니다.
