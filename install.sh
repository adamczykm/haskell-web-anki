cd backend
stack install
cd ../frontend
stack install
cd bin
ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js

cd ../../electron-build
rm -f renderer.js
rm -f backend-exe

ln -s $(find ../backend/.stack-work -name backend-exe | tail -n 1) backend-exe
ln -s $(find ../frontend -name all.min.js) renderer.js
