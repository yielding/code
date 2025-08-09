#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <functional>
#include <sstream>
#include <numeric>
#include <algorithm>
#include <ranges>
#include <expected>
#include <variant>
#include <map>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
// 1. Parser Combinator - 간단하고 실용적인 수준
//    CSV 파싱 같은 실제 문제 해결
//
////////////////////////////////////////////////////////////////////////////////

class SimpleParser {
public:
    using Result = optional<pair<string, string>>;  // (parsed, remaining)
    
    // 기본 파서: 한 글자 파싱
    static function<Result(string)> char_(char c) {
        return [c](string input) -> Result {
            if (!input.empty() && input[0] == c) {
                return make_pair(string(1, c), input.substr(1));
            }
            return nullopt;
        };
    }
    
    // 숫자 파싱
    static function<Result(string)> digit() {
        return [](string input) -> Result {
            if (!input.empty() && isdigit(input[0])) {
                return make_pair(string(1, input[0]), input.substr(1));
            }
            return nullopt;
        };
    }
    
    // 여러 자리 정수 파싱
    static function<Result(string)> integer() {
        return [](string input) -> Result {
            size_t i = 0;
            while (i < input.size() && isdigit(input[i])) i++;
            if (i > 0) {
                return make_pair(input.substr(0, i), input.substr(i));
            }
            return nullopt;
        };
    }
    
    // 파서 합성: 순차 실행 (>>=)
    template<typename P1, typename P2>
    static auto sequence(P1 p1, P2 p2) {
        return [p1, p2](string input) -> Result {
            if (auto r1 = p1(input)) {
                auto [v1, rest1] = *r1;
                if (auto r2 = p2(rest1)) {
                    auto [v2, rest2] = *r2;
                    return make_pair(v1 + v2, rest2);
                }
            }
            return nullopt;
        };
    }
    
    // 파서 선택: 첫 번째 성공하는 파서 사용 (|)
    template<typename P1, typename P2>
    static auto choice(P1 p1, P2 p2) {
        return [p1, p2](string input) -> Result {
            if (auto r = p1(input)) return r;
            return p2(input);
        };
    }
    
    // 실용적인 CSV 라인 파서
    static vector<string> parseCSVLine(const string& line) {
        vector<string> result;
        string current;
        
        for (size_t i = 0; i < line.size(); ++i) {
            if (line[i] == ',') {
                result.push_back(current);
                current.clear();
            } else {
                current += line[i];
            }
        }
        if (!current.empty()) result.push_back(current);
        
        return result;
    }
};

void parserExample() {
    cout << "=== Parser Combinator Example ===\n";
    
    // 간단한 날짜 파서: YYYY-MM-DD
    auto year = SimpleParser::integer();
    auto dash = SimpleParser::char_('-');
    auto month = SimpleParser::integer();
    auto day = SimpleParser::integer();
    
    auto dateParser = SimpleParser::sequence(
        SimpleParser::sequence(
            SimpleParser::sequence(
                SimpleParser::sequence(year, dash),
                month
            ),
            dash
        ),
        day
    );
    
    string date = "2024-12-09-rest";
    if (auto result = dateParser(date)) {
        auto [parsed, remaining] = *result;
        cout << "Parsed date: " << parsed << "\n";
        cout << "Remaining: " << remaining << "\n";
    }
    
    // CSV 파싱
    string csvLine = "John,30,Engineer";
    auto fields = SimpleParser::parseCSVLine(csvLine);
    cout << "CSV fields: ";
    for (const auto& field : fields) {
        cout << "[" << field << "] ";
    }
    cout << "\n\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 2. Query Builder - SQL 쿼리를 타입 안전하게 구성
//    실제 ORM에서 사용하는 패턴
//
////////////////////////////////////////////////////////////////////////////////

class QueryBuilder {
private:
    string table_;
    vector<string> select_columns_;
    vector<string> where_conditions_;
    optional<string> order_by_;
    optional<int> limit_;
    
public:
    QueryBuilder& from(const string& table) {
        table_ = table;
        return *this;
    }
    
    QueryBuilder& select(const vector<string>& columns) {
        select_columns_ = columns;
        return *this;
    }
    
    QueryBuilder& where(const string& condition) {
        where_conditions_.push_back(condition);
        return *this;
    }
    
    QueryBuilder& orderBy(const string& column, bool ascending = true) {
        order_by_ = column + (ascending ? " ASC" : " DESC");
        return *this;
    }
    
    QueryBuilder& limit(int n) {
        limit_ = n;
        return *this;
    }
    
    string build() const {
        stringstream ss;
        
        // SELECT
        ss << "SELECT ";
        if (select_columns_.empty()) {
            ss << "*";
        } else {
            for (size_t i = 0; i < select_columns_.size(); ++i) {
                if (i > 0) ss << ", ";
                ss << select_columns_[i];
            }
        }
        
        // FROM
        ss << " FROM " << table_;
        
        // WHERE
        if (!where_conditions_.empty()) {
            ss << " WHERE ";
            for (size_t i = 0; i < where_conditions_.size(); ++i) {
                if (i > 0) ss << " AND ";
                ss << where_conditions_[i];
            }
        }
        
        // ORDER BY
        if (order_by_) {
            ss << " ORDER BY " << *order_by_;
        }
        
        // LIMIT
        if (limit_) {
            ss << " LIMIT " << *limit_;
        }
        
        return ss.str();
    }
    
    // 실행 시뮬레이션 (실제로는 DB 연결 사용)
    expected<vector<map<string, string>>, string> execute() const {
        string query = build();
        cout << "Executing: " << query << "\n";
        
        // 시뮬레이션된 결과
        if (table_ == "users") {
            return vector<map<string, string>>{
                {{"id", "1"}, {"name", "Alice"}, {"age", "30"}},
                {{"id", "2"}, {"name", "Bob"}, {"age", "25"}}
            };
        }
        return unexpected("Table not found");
    }
};

void queryBuilderExample() {
    cout << "=== Query Builder Example ===\n";
    
    // 메서드 체이닝으로 쿼리 구성
    QueryBuilder qb;
    auto query = qb
        .from("users")
        .select({"id", "name", "age"})
        .where("age > 21")
        .where("status = 'active'")
        .orderBy("name")
        .limit(10)
        .build();
    
    cout << "Built query: " << query << "\n";
    
    // 실행 및 결과 처리
    auto result = qb.from("users").select({"name", "age"}).execute();
    if (result) {
        cout << "Query results:\n";
        for (const auto& row : *result) {
            for (const auto& [col, val] : row) {
                cout << col << ": " << val << " ";
            }
            cout << "\n";
        }
    }
    cout << "\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 3. Data Pipeline - 데이터 변환 파이프라인
//    실제 ETL, 스트림 처리에서 사용
//
////////////////////////////////////////////////////////////////////////////////

template<typename T>
class Pipeline {
private:
    vector<function<T(T)>> transformations_;
    
public:
    // 변환 추가
    Pipeline& transform(function<T(T)> f) {
        transformations_.push_back(f);
        return *this;
    }
    
    // 필터링 (조건을 만족하지 않으면 optional::nullopt)
    Pipeline& filter(function<bool(const T&)> predicate) {
        transformations_.push_back([predicate](T data) -> T {
            // 실제로는 optional이나 variant를 사용하는 것이 좋음
            return predicate(data) ? data : T{};
        });
        return *this;
    }
    
    // 파이프라인 실행
    T process(T input) const {
        T result = input;
        for (const auto& transform : transformations_) {
            result = transform(result);
        }
        return result;
    }
    
    // 배치 처리
    vector<T> processBatch(const vector<T>& inputs) const {
        vector<T> results;
        for (const auto& input : inputs) {
            results.push_back(process(input));
        }
        return results;
    }
};

// 실용적인 예: 로그 데이터 처리 파이프라인
struct LogEntry {
    string timestamp;
    string level;
    string message;
    int count = 1;
};

class LogPipeline {
public:
    static Pipeline<LogEntry> createProcessingPipeline() {
        Pipeline<LogEntry> pipeline;
        
        return pipeline
            // 타임스탬프 정규화
            .transform([](LogEntry log) {
                log.timestamp = log.timestamp.substr(0, 10);  // 날짜만 추출
                return log;
            })
            // 에러 레벨 대문자 변환
            .transform([](LogEntry log) {
                transform(log.level.begin(), log.level.end(), 
                         log.level.begin(), ::toupper);
                return log;
            })
            // 메시지 길이 제한
            .transform([](LogEntry log) {
                if (log.message.length() > 50) {
                    log.message = log.message.substr(0, 47) + "...";
                }
                return log;
            });
    }
    
    // C++20 ranges를 활용한 더 현대적인 파이프라인
    static vector<string> processWithRanges(const vector<LogEntry>& logs) {
        namespace rv = std::ranges::views;
        
        auto result = logs 
            | rv::filter([](const LogEntry& log) { 
                return log.level == "ERROR" || log.level == "WARN"; 
              })
            | rv::transform([](const LogEntry& log) {
                return "[" + log.timestamp + "] " + log.level + ": " + log.message;
              });
        
        return vector<string>(result.begin(), result.end());
    }
};

void pipelineExample() {
    cout << "=== Pipeline Processing Example ===\n";
    
    // 숫자 처리 파이프라인
    Pipeline<int> numberPipeline;
    numberPipeline
        .transform([](int x) { return x * 2; })     // 2배
        .transform([](int x) { return x + 10; })    // +10
        .transform([](int x) { return x / 3; });    // /3
    
    cout << "Number pipeline: 15 -> " << numberPipeline.process(15) << "\n";
    
    // 로그 처리 파이프라인
    vector<LogEntry> logs = {
        {"2024-12-09 10:15:30", "error", "Connection failed to database server after multiple retries", 1},
        {"2024-12-09 10:16:00", "info", "Service started", 1},
        {"2024-12-09 10:16:30", "warn", "Memory usage above 80%", 1}
    };
    
    auto logPipeline = LogPipeline::createProcessingPipeline();
    cout << "\nProcessed logs:\n";
    for (const auto& log : logs) {
        auto processed = logPipeline.process(log);
        cout << processed.timestamp << " | " << processed.level 
             << " | " << processed.message << "\n";
    }
    
    // Ranges 파이프라인
    cout << "\nFiltered error/warn logs:\n";
    auto filtered = LogPipeline::processWithRanges(logs);
    for (const auto& line : filtered) {
        cout << line << "\n";
    }
    cout << "\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// Main - 모든 예제 실행
//
////////////////////////////////////////////////////////////////////////////////

int main() {
    cout << "=== Practical Domain-Specific Function Composition ===\n";
    cout << "These examples show appropriate use of functional patterns\n";
    cout << "without over-engineering.\n\n";
    
    parserExample();
    queryBuilderExample();
    pipelineExample();
    
    cout << "=== Key Takeaways ===\n";
    cout << "1. Parser Combinators: Great for DSLs and text processing\n";
    cout << "2. Query Builders: Type-safe database queries\n";
    cout << "3. Pipeline Processing: Clean data transformation chains\n\n";
    
    cout << "These patterns provide:\n";
    cout << "✅ Clear intent and readability\n";
    cout << "✅ Type safety and composability\n";
    cout << "✅ Testability and maintainability\n";
    cout << "✅ Performance (no std::function overhead when using templates)\n";
    
    return 0;
}