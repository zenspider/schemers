
task :clean do
  rm_f Dir["**/*~"]
end

task :test do
  Dir["*.scm"].each do |file|
    sh "mzscheme #{file}"
  end
end
