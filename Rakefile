task :default => [:clean, :test]

task :clean do
  rm_f Dir["**/*~"]
end

task :test do
  touch_file = ".gitignore"
  t0 = File.mtime(touch_file) rescue Time.at(0)

  Dir["*.scm"].each do |file|
    t1 = File.mtime(file)
    next if t0 > t1
    sh "X=1 time racket -f #{file} 2>&1"
  end

  touch touch_file
end
