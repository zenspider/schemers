task :default => [:clean, :test]

task :clean do
  rm_f Dir["**/*~"]
end

task :test do
  Dir["*.scm"].each do |file|
    sh "X=1 time racket -f #{file} 2>&1"
  end
end
