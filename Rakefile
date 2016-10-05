require 'pathname'

desc "*.md => docs/*.html"
task :to_docs, :f do |task, args|
  md = args[:f]
  html = File.join(["docs", File.basename(md, ".md")]) + ".html"
  system("pandoc", *[md, "-s", "-t" "html5", "-c", "stylesheet.css", "-o", html])
end
