task :default => [:clean, :test]

task :clean do
  rm_f Dir["**/*~"]
end

def with_newer_files touch_file = ".gitignore"
  t0 = File.mtime(touch_file) rescue Time.at(0)

  Dir["**/*.scm"].each do |file|
    t1 = File.mtime(file)
    next if t0 > t1
    yield file
  end
end

def update_touch_file touch_file = ".gitignore"
  touch touch_file
end

task :todo do
  system "grep -l TODO **/*.scm"
end

task :run do
  with_newer_files do |file|
    sh "X=1 time racket #{file} 2>&1"
  end
end

task :test do
  with_newer_files do |file|
    sh "X=1 time racket -f #{file} 2>&1"
  end

  update_touch_file
end

task :autotest do
  sh "run_if_changed 'rake run' */*.scm"
end
