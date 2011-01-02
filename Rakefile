task :default => [:clean, :test]

task :clean do
  rm_f Dir["**/*~"]
end

def newer_files touch_file = ".gitignore"
  t0 = File.mtime(touch_file) rescue Time.at(0)

  Dir["**/*.scm"].find_all { |file| t0 <= File.mtime(file) }
end

def with_newer_files touch_file = ".gitignore"
  t0 = File.mtime(touch_file) rescue Time.at(0)

  newer_files(touch_file).each do |file|
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

task :debug do
  sh "drracket #{newer_files.join(" ")}"
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
