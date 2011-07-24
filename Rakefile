task :default => [:clean, :test]

task :clean do
  rm_f Dir["**/*~"]
end

def newer_files touch_file = ".gitignore"
  t0 = File.mtime(touch_file) rescue Time.at(0)

  Dir["{ch*,little-schemer}/*.scm"].find_all { |file| t0 <= File.mtime(file) }
end

def with_newer_files touch_file = ".gitignore"
  t0 = File.mtime(touch_file) rescue Time.at(0)

  newer_files(touch_file).each do |file|
    yield file
  end
end

def update_touch_file touch_file = ".gitignore"
  open touch_file, "w" do |f|
    f.puts ".gitignore"
  end
end

task :todo do
  system "grep TODO **/*.scm"
end

task :run do
  size = (ENV["SIZE"] || 25).to_i
  with_newer_files do |file|
    sh "X=1 time timeout csi -q -:a#{size} -I lib -I ../lib < #{file} 2>&1"
  end
end

task :lint, [:f] do |t, args|
  f = args.f
  sh "csc -to-stdout -S #{f} > /dev/null"
end

task :debug do
  sh "drracket #{newer_files.join(" ")}"
end

task :test do
  with_newer_files do |file|
    sh "X=1 time timeout csi -I lib -I ../lib -s #{file} 2>&1"
  end

  update_touch_file
end

task :autotest do
  sh "run_if_changed 'rake run' */*.scm"
end

task :split do
  exercises = File.read("x.scm").split(/;; .(Exercise \d+\.\d+):/)
  exercises.shift # nuke the empty at the front
  exercises.each_slice(2) do |x, y|
    ch, ex = x.scan(/\d+/).map { |s| s.to_i }

    path = "ch_%d/exercise.%d.%02d.scm" % [ch, ch, ex]

    if File.exist? path then
      warn "File #{path} exists. Skipping."
      next
    end

    y.gsub!(/(;;\n)+\Z/, '')
    y.gsub!(/\A\*\s+/, '')

    File.open path, "w" do |f|
      f.puts "#!/usr/bin/env csi -s"
      f.puts
      f.puts "(use test)"
      f.puts
      f.puts ";;; #{x}"
      f.puts
      f.puts ";; #{y}"
    end
  end
end

task :commit, [:n] => [:default] do |t, args|
  n = args.n
  f = Dir["*/*#{n}.scm"].first
  sh "git commit -m #{n.inspect} #{f}"
end

task :logic do
  sh "csi -q -I lib -I ../lib ../bin/logic.scm"
end
